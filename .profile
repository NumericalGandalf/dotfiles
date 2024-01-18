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

export EDITOR=nvim
export TERMINAL=kitty
export BROWSER=firefox

export JAVA_HOME=/usr/lib/jvm/java-21-openjdk
export GOPATH="$HOME/go"

TERMCLT_MAX=2
if [[ -z $DISPLAY ]]; then
	TERMCLT="$(tty | rev | cut -c 1)"
	if [[ $TERMCLT -lt $TERMCLT_MAX ]]; then
		exec startx >/dev/null 2>&1
	fi
else
	USE_P10K=1
	export XDG_SESSION_TYPE=x11
fi
