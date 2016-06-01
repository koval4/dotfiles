#!/bin/sh

#colors
blue="#71a2df"
gray="#121212"
BG="#0b0b14"
FG="#e6f7ff"
FONT="M+1mn:size=10" # "-*-terminus-medium-r-*-*-10-*-*-*-*-*-iso10646-*"

#BAR_OPS="-fg #d9d9d9 -bg #555555 -h 7 -w 25 -s o -ss 1 -sw 2"
BAR_OPS="-fg #d9d9d9 -bg $blue -h 7 -w 40 -s o -nonl"

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

# spacing
space(){
	echo "^fn($FONT)^fg(#555555) | ^fg()^fn()"
}

# shows current keyboard layout
# left click to change layout to us -> ua -> ru -> us ...
layout(){
	LAYOUT=$(setxkbmap -query | awk 'END{print $2}')
	#shows current keyboard layout
	#changes layout whel clicked
    echo -n "^ca(1, $HOME/scripts/layout_switch.sh)^fg()$LAYOUT^fg()^ca() ^fg($blue)^i($PEN)^fg()"
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
	ismute=`amixer get Master|grep %|awk '{ print $6 }'|sed 's/\[//g'|sed 's/\]//g'`
if [ "$ismute" == "off" ]; then
		VBS="0"
	else
		VBS=`amixer get Master|grep %|awk '{ print $4 }'|sed 's/%//g'|sed 's/\[//g'|sed 's/\]//g'`
	fi	
	
    VBAR=`echo "$VBS" | gdbar $BAR_OPS |awk '{ print $1 }'`
	echo "^ca(4, amixer set Master 2dB+)^ca(5, amixer set Master 2dB-)^fg($blue)^i($VOL) ^fg()$VBAR ^ca()^ca()"
}

# shows current date and time
dateTime(){
	TIME=`date +"%H:%M:%S"`
	#CALENDAR=cal | dzen2 -x 1240 -w 200 -h 150 -e 'onstart=uncollapse;button1=exit;button3=exit'
	echo "^ca(1, $HOME/scripts/calendar.sh)^fg($blue)^i($CLOCK) ^fg()$TIME^ca()"
}

tasker(){
    TASKER_PATH="$HOME/scripts/tasker"
    TODO_PATH="$HOME/todo.txt"
    TODO_ITEM=`$TASKER_PATH view $TODO_PATH`
    REMOVE="$TASKER_PATH remove $TODO_PATH"
    ADD="xterm -e $TASKER_PATH add $TODO_PATH"
    VIEW="xterm -e nvim $TODO_PATH"
    CONTROLS="^fg($blue)[^fg()^ca(1, $REMOVE)✔^ca()|^ca(1, $ADD)+^ca()^fg($blue)]^fg()"
    echo "^fg($blue)TODO:^fg() ^ca(1, $VIEW)$TODO_ITEM^ca() $CONTROLS"
}

while true ; do
    echo $(tasker)$(space)$(layout)$(space)$(sysinfo)$(space)$(vol)$(music)$(space)$(dateTime)$(space)	
    sleep 1
done | dzen2 -p -x 740 -w 700 -h 25 -ta 'r' -fg $FG -bg $BG -fn "$FONT" -e 'button2=;' 
