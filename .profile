#!/usr/bin/bash

if local_bin="$HOME/.local/bin"; [[ ":$PATH:" != *":$local_bin:"* ]]; then
    export PATH="$PATH:$local_bin"
fi
if go_bin="$HOME/go/bin"; [[ ":$PATH:" != *":$go_bin:"* ]]; then
    export PATH="$PATH:$go_bin"
fi

export EDITOR=/usr/bin/nvim
export TERMINAL="alacritty"
export "$(dbus-launch)"

export JAVA_HOME=/usr/lib/jvm/java-21-openjdk
export GOPATH="$HOME/go"

if [[ -n $I3SOCK ]]; then
    alias xdg-open="i3-msg 'exec --no-startup-id xdg-open $@'"
fi

if [[ -z $DISPLAY ]]; then
    TERMCLT="$(tty | rev | cut -c 1)"
    export TERMCLT
    if [ "$TERMCLT" -lt 3 ]; then
        exec startx > /dev/null 2>&1
    fi
else
    export XDG_SESSION_TYPE=x11
fi

