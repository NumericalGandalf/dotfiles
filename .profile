#!/usr/bin/bash

if local_bin="$HOME/.local/bin"; [[ ":$PATH:" != *":$local_bin:"* ]]; then
    export PATH="$PATH:$local_bin"
fi

export EDITOR=/usr/bin/nvim
export TERMINAL="alacritty"
export $(dbus-launch)

export JAVA_HOME=/usr/lib/jvm/java-21-openjdk

if [[ -z $DISPLAY ]]; then
    export TERMCLT="${$(tty):0-1}"
    if [ $TERMCLT -lt 3 ]; then
        exec startx > /dev/null 2>&1
    fi
else
    export XDG_SESSION_TYPE=x11
fi

