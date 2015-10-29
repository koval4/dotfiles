#!/bin/sh

BG="#121212"
FG="#d9d9d9"
CURE="-*-Ohsnap-*-*-*-*-9-*-*-*-*-*-*-*"
SNAP="-*-Ohsnap-*-*-*-*-9-*-*-*-*-*-*-*"

BAR_OPS="-fg #d9d9d9 -bg #555555 -h 7 -w 25 -s o -ss 1 -sw 2"

# Icons
CLOCK="$HOME/.xmonad/dzen_icons/clock.xbm"
VOL="$HOME/.xmonad/dzen_icons//spkr_01.xbm"

CPU="$HOME/.xmonad/dzen_icons//cpu.xbm"
MEM_ICO="$HOME/.xmonad/dzen_icons//mem.xbm"
FS="$HOME/.xmonad/dzen_icons//fs_02.xbm"
MUSIC="$HOME/.dzen/xbm/note.xbm"
KEYBOARD_ICO="$HOME/.xmonad/dzen_icons//kboard.xbm"

NOW_PLAYING_FORMAT="%a - %t"

#colors
blue="#3399ff"
gray="#121212"

space(){
	echo "^fn($SNAP)^fg(#555555) | ^fg()^fn()"
}

layout(){
	LAYOUT=$(setxkbmap -query | awk 'END{print $2}')
	#shows current keyboard layout
	#changes layout whel clicked
	echo -n "^ca(1, $HOME/scripts/layout_switch.sh)^fg()$LAYOUT^fg()^ca()"
}

load() {
	cpu=$(bc <<< $(ps -eo pcpu |grep -vE '^\s*(0.0|%CPU)' |sed -n '1h;$!H;$g;s/\n/ +/gp'))
	#cpu=$($HOME/scripts/cpu_usage.sh)
	echo -ne "^fg($blue)^i($CPU) ^fg()$cpu%"
}

mem(){
	MEM=$(free -m | grep Mem: '-' | awk '{print $3,"/",$2}')
	#echo -n "^fg($COLOR_ICON)^i($ICONPATH/mem1.xbm)^fg() ${MEM} MB"
	echo -n "^fg($blue)^i($MEM_ICO) ^fg()${MEM} MB"
	return
}

music(){
	playing=$(mpc current)
	echo -n "^ca(3, mpc toggle) ^ca(4, mpc next) ^ca(5, mpc prev) ^fg($blue)[^fg()$playing^fg($blue) ] ^ca() ^ca() ^ca()"
}

vol(){
	ismute=`amixer get Master|grep %|awk '{ print $6 }'|sed 's/\[//g'|sed 's/\]//g'`
	if [ "$ismute" == "off" ]; then
		VBS="0"
	else
		VBS=`amixer get Master|grep %|awk '{ print $4 }'|sed 's/%//g'|sed 's/\[//g'|sed 's/\]//g'`
	fi	
	
	VBAR=`echo "$VBS" | gdbar $BAR_OPS |awk '{ print $1 }'`
	echo "^fg($blue)^i($VOL)^fg() $VBAR"
}

dateTime(){
	DATE=`date +"%d %b %A,"`
	TIME=`date +"%I:%M:%S"`
	#CALENDAR=cal | dzen2 -x 1240 -w 200 -h 150 -e 'onstart=uncollapse;button1=exit;button3=exit'
	echo "^ca(1, cal) ^fg($blue)^i($CLOCK)^fg() $DATE $TIME ^ca()"
}

while true ; do
	echo $(space)$(layout)$(space)$(load)$(mem)$(space)$(vol)$(music)$(space)$(dateTime)$(space)	
done | dzen2 -p -x 740 -w 700 -h 20 -ta 'r' -fg $FG -bg $BG -fn $CURE -e 'button1=exec:xterm; button2=;' 
