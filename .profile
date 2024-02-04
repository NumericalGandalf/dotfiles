#!/usr/bin/bash

pathappend() {
	for ARG in "$@"; do
		if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
			PATH="${PATH:+"$PATH:"}$ARG"
		fi
	done
}

defaults() {
	export EDITOR=nvim
	export TERMINAL=kitty
	export BROWSER=vivaldi

	export JAVA_HOME=/usr/lib/jvm/java-17-openjdk
	export GOPATH=$HOME/go
}

display() {
	termclt="$(tty | rev | cut -c 1)"
	if [[ $termclt -gt 3 ]]; then
		return 0
	fi

	wayland=0
	if [[ $wayland -eq 0 ]]; then
		export XDG_SESSION_TYPE=x11
	else
		export XDG_SESSION_TYPE=wayland
	fi

	if [[ -n $DISPLAY || -n $WAYLAND_DISPLAY ]]; then
		return 0
	fi

	if [[ $wayland -eq 0 ]]; then
		session=i3
		if [[ $termclt -eq 2 ]]; then
			session=kde
		else
			[[ $termclt -eq 3 ]]
			session=gnome
		fi
		xinit $session
	fi
}

main() {
	pathappend $HOME/.local/bin $HOME/go/bin
	defaults
	display
}

main
