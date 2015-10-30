#!/bin/bash

TODAY=$(expr `date +'%d'` + 0)
MONTH=`date +'%m'`
YEAR=`date +'%Y'`

(

# current month, hilight header and today
cal | sed -re "s/^(.*[A-Za-z][A-Za-z]*.*)$/^fg(#DE8834)^bg(#222222)\1/;
s/(^|[ ])($TODAY)($|[ ])/\1^bg(#DE8834)^fg(#222222)\2^fg(#6c6c6c)^bg(#222222)\3/"
sleep 8
#cal
) | dzen2 -p -x 1240 -w 200 -h 150 -l 10 -e 'onstart = uncollapse; button1 = exit; button3 = exit;'

