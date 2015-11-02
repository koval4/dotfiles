#!/bin/bash

BG="#121212"
FG="#d9d9d9"
FONT="M+1mn:size=8"

#colors
blue="#3399ff"

#date formats
TODAY=$(expr `date +'%d'` + 0)
MONTH=`date +'%m'`
YEAR=`date +'%Y'`

(
echo "^fg($blue)`date +'%d %b %H.%M'` ^fg()"

# current month, hilight header and today
cal | sed -re "s/^(.*[A-Za-z][A-Za-z]*.*)$/^fg($blue)\1/;
s/(^|[ ])($TODAY)($|[ ])/\1^fg()\2^fg()\3/"
sleep 8
) | dzen2 -x 1290 -y 20 -w 130 -l 7 -fn $FONT -fg $FG -bg $BG
