#!/usr/bin/sh

xrandr --output DP-1 --mode 2560x1440 --rate 165
setxkbmap -option caps:swapescape

xss-lock --transfer-sleep-lock -- i3lock -c 000000 &

[[ -n "$1" ]] && exec $1
