#!/bin/sh

#colors
blue="#71a2df"
gray="#121212"
BG="#0b0b14"
FG="#e6f7ff"
FONT="M+1mn:size=10"

# Icons
ICONS_PATH="$HOME/.xmonad/dzen_icons" # path to icons directory
PEN="$ICONS_PATH/art.xbm"
CLOCK="$ICONS_PATH/clock1.xbm"
VOL="$ICONS_PATH/spkr_01.xbm"
CPU="$ICONS_PATH/cpu.xbm"
MEM_ICO="$ICONS_PATH/mem.xbm"
NEXT_ICO="$ICONS_PATH/next.xbm"
PREV_ICO="$ICONS_PATH/prev.xbm"
PLAY_ICO="$ICONS_PATH/play.xbm"
PAUSE_ICO="$ICONS_PATH/pause.xbm"

NOW_PLAYING_FORMAT="%a - %t"

xmonad_info() {
	cat /tmp/xmonad
}

# spacing
space(){
	echo "%{F#555} | %{F-}"
}

# shows current keyboard layout
# left click to change layout to us -> ua -> ru -> us ...
layout(){
	LAYOUT=$(setxkbmap -query | awk 'END{print $2}')
	#shows current keyboard layout
	#changes layout whel clicked
    echo -n "%{A:$HOME/scripts/layout_switch.sh:}$LAYOUT%{A}"
}

# makes clickable area
# if clicked -> shows conky
sysinfo() {
    echo "^ca(1, conky -i 30)^fg($blue)^i($CPU)^fg()^ca()"
}

# shows current system load
load() {
	cpu=$(bc <<< $(ps -eo pcpu |grep -vE '^\s*(0.0|%CPU)' |sed -n '1h;$!H;$g;s/\n/ +/gp'))
	#cpu=$($HOME/scripts/cpu_usage.sh)
	echo -ne "^fg($blue)^i($CPU) ^fg()$cpu%"
}

# shows RAM usage in format used_ram / total_ram MB
mem(){
	MEM=$(free -m | grep Mem: '-' | awk '{print $3,"/",$2}')
	echo -n "^fg($blue)^i($MEM_ICO) ^fg()${MEM} MB"
	return
}

# shows buttons for mpd controls
music(){
    echo -n "^fg($blue)^ca(1, mpc prev)^i($PREV_ICO)^ca() ^ca(1, mpc play)^i($PLAY_ICO)^ca() ^ca(1, mpc pause)^i($PAUSE_ICO)^ca() ^ca(1, mpc next)^i($NEXT_ICO)^ca()^fg()" 
}

# shows volume bar
# scroll up to increase volume
# scroll downto decrease volume
vol(){
	#BAR_OPS="-fg #d9d9d9 -bg #555555 -h 7 -w 25 -s o -ss 1 -sw 2"
	BAR_OPS="-fg #d9d9d9 -bg $blue -h 7 -w 40 -s o -nonl"

    cur=$(amixer get Master|grep %|awk '{ print $4 }'|sed 's/%//g'|sed 's/\[//g'|sed 's/\]//g')
    tot=100

    for w in `seq 0 10 $((cur - 1))`; do line="${line}━"; done
    for w in `seq $((cur + 1)) 10 $tot`; do line="${line}─"; done

    VBAR=`echo "$VBS" | gdbar $BAR_OPS |awk '{ print $1 }'`
	echo -e "%{A4:amixer set Master 2dB+:}%{A5:amixer set Master 2dB-:}%{F${blue}}\uf028 %{F-}$line %{A}%{A}"
}

# shows current date and time
dateTime(){
    clock="%{F${blue}}\uf017%{F-}"
	TIME=`date +"%H:%M"`
	echo -e "%{A:$HOME/scripts/calendar.sh:}$TIME%{A}"
}

tasker(){
    TASKER_PATH="$HOME/scripts/tasker"
    TODO_PATH="$HOME/todo.txt"
    TODO_ITEM=`$TASKER_PATH view $TODO_PATH`
    REMOVE="$TASKER_PATH remove $TODO_PATH"
    ADD="xterm -e $TASKER_PATH add $TODO_PATH"
    VIEW="xterm -e nvim $TODO_PATH"
    CONTROLS="%{F${blue}}[%{F-}%{A:$REMOVE:}✔%{A}|%{A:$ADD:}+%{A}%{F${blue}}]%{F-}"
    echo "%{F${blue}}TODO:%{F-} %{A:$VIEW:}$TODO_ITEM%{A} $CONTROLS"
}

while true ; do
	#    echo $(tasker)$(space)$(layout)$(space)$(sysinfo)$(space)$(vol)$(music)$(space)$(dateTime)$(space)
	echo "%{l}%{c}%{r}$(tasker)$(space)$(layout)$(space)$(vol)$(space)$(dateTime)$(space)"
    sleep 1
done | lemonbar -p -d -g 766x25+600+0  -F $FG -B $BG -f "$FONT" -f "FontAwesome:size=10" | zsh
