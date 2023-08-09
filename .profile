#!/usr/bin/sh

if local_bin="$HOME/.local/bin"; [[ ":$PATH:" != *":$local_bin:"* ]]; then
    export PATH="$PATH:$local_bin"
fi

export TERMINAL="alacritty"

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx > /dev/null 2>&1 &
fi
