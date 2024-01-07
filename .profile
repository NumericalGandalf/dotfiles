#!/usr/bin/bash

if
	local_bin="$HOME/.local/bin"
	[[ ":$PATH:" != *":$local_bin:"* ]]
then
	export PATH="$PATH:$local_bin"
fi
if
	go_bin="$HOME/go/bin"
	[[ ":$PATH:" != *":$go_bin:"* ]]
then
	export PATH="$PATH:$go_bin"
fi

export $(dbus-launch)
export EDITOR=nvim
export TERMINAL=alacritty
export BROWSER=firefox

export JAVA_HOME=/usr/lib/jvm/java-21-openjdk
export GOPATH="$HOME/go"

if [[ -z $DISPLAY ]]; then
	TERMCLT="$(tty | rev | cut -c 1)"
	export TERMCLT
	if [ "$TERMCLT" -lt 2 ]; then
		exec startx >/dev/null 2>&1
	fi
else
	export XDG_SESSION_TYPE=x11
fi
