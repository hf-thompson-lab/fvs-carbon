#!/bin/sh
egrep -h "(THE DIAMETER INCREMENT MODEL)|(DATA BASE CONTROL NUMBER)" FVS_Northeastern_NONE_?.out > calibration.txt
sed -E -e '/STANDCN/ { h          
d
}' -e '/THE DIAMETER/ { H
x
}' -e 's/STANDCN    DATA BASE CONTROL NUMBER=([0-9]+)../\1 /' -e 's/THE DIAMETER INCREMENT MODEL//' -e 's/ +/ /g' -e 's/^([0-9]+) 0[ 0]+/\1 0/g' -e 's/^([0-9]+) [0-9]+( [0-9]+)+/\1 1/g' calibration.txt > calibration.csv
