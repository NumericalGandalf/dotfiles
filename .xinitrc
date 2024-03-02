#!/usr/bin/sh

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

command -v dunst && dunst &

# command -v picom && picom &

command -v xss-lock xsecurelock &&
  xss-lock --transfer-sleep-lock -- xsecurelock &

command -v emacs && emacs --daemon

[[ -n "$1" ]] && command -v $1 && session=$1 ||
  session=i3 && exec $session
