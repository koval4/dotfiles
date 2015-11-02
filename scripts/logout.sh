#! /bin/bash

#colors
BG="#121212"
FG="#d9d9d9"
blue="#3399ff"

#font
FONT="-*-0hsnap-*-*-*-*-9-*-*-*-*-*-*-*"

show(){
	echo "Log out"
	echo "^ca(1, reboot) Reboot ^ca()"
	echo "^ca(1, poweroff) Shut down ^ca"
	sleep 5
}

($show) | dzen2 -x 1200 -y 20 -w 100 -l 3 -fn $FONT -fg $FG -bg $BG

