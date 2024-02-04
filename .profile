#!/usr/bin/bash

export PATH="$PATH:$local_bin"
export PATH="$PATH:$go_bin"

export EDITOR=nvim
export TERMINAL=kitty
export BROWSER=vivaldi

export JAVA_HOME=/usr/lib/jvm/java-17-openjdk
export GOPATH="$HOME/go"

if [[ -z $DISPLAY ]]; then
	export TERMCLT="$(tty | rev | cut -c 1)"
	if [[ $TERMCLT -lt 4 ]]; then
		exec startx
	fi
else
	export XDG_SESSION_TYPE=x11
fi
