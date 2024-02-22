#!/usr/bin/sh

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

[[ -n "$1" && $(command -v $1) ]] && session=$1 ||
  session=i3 && exec $session
