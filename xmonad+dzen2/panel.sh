
#!/bin/sh

#colors
blue="#71a2df"
gray="#121212"
BG="#0b0b14"
FG="#e6f7ff"
FONT="M+1mn:size=10"

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
    echo -ne "%{A:$HOME/scripts/layout_switch.sh:}%{F${blue}}\uf044%{F-} $LAYOUT%{A}"
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

# shows current date and time
dateTime(){
    clock="%{F${blue}}\uf017%{F-}"
	TIME=`date +"%H:%M"`
	echo -e "%{A:$HOME/scripts/calendar.sh:}$TIME%{A}"
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

echo -e "%{l}%{c}%{r}$(tasker)$(space)$(layout)$(space)$(vol)$(space)$(dateTime)$(space)"
sleep 1
