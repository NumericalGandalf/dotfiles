#!/usr/bin/sh

xmodmap ~/.Xmodmap
xrdb -merge ~/.Xresources

dunst &
picom &
xss-lock --transfer-sleep-lock -- xsecurelock &

[[ -n "$1" ]] && command -v $1 && session=$1 ||
  session=i3 && exec $session
