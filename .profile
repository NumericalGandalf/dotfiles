#!/usr/bin/sh

local_bin="$HOME/.local/bin"
if [[ ":$PATH:" != *":$local_bin:"* ]]; then
    export PATH="$PATH:$local_bin"
fi

export TERMINAL="alacritty"

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx &
fi
