#!/usr/bin/sh

xrandr --output DP-1 --mode 1920x1080 --rate 144
setxkbmap -option caps:swapescape

dunst &

xss-lock --transfer-sleep-lock -- i3lock -c 000000 &

[[ -n "$1" ]] && command -v $1 && session=$1 ||
  session=i3 && exec $session
