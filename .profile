#!/usr/bin/bash

if local_bin="$HOME/.local/bin"; [[ ":$PATH:" != *":$local_bin:"* ]]; then
    export PATH="$PATH:$local_bin"
fi

export EDITOR=/usr/bin/nvim
export TERMINAL="alacritty"

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk

if [[ -z $DISPLAY ]]; then
    export TERMCLT="${$(tty):0-1}"
    exec startx > /dev/null 2>&1
fi

export XDG_SESSION_TYPE=x11
