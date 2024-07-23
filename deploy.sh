#!/usr/bin/sh

emacs_dir="$HOME/.emacs.d"

function fail() {
    echo "Error:" "$@" >&2
    exit 1
}

function requires() {
    local util=$(command -v $1)
    [[ -z "$util" ]] && fail "Requires utility" $1
}

function requires_shell() {
    requires $1
    [[ "$SHELL" != "/bin/${1}" ]] && fail "Requires shell" $1
}

function requires_emacs_version() {
    local version=$(emacs --version | head -n 1 | awk '{print $3}')
    [[ $(printf '%s\n' $1 $version | sort -V | head -n1) != $1 ]] &&
	fail "Requires at least Emacs version" $1
}

function unlink_emacs_dir() {
    [[ -L $emacs_dir ]] && rm $emacs_dir
    [[ -d $emacs_dir ]] && rm -rf $emacs_dir
}

function link_emacs_dir() {
    local emacs_file="$HOME/.emacs"
    local dot_emacs_dir=$(realpath emacs)
    [[ ! -d $dot_emacs_dir ]] && fail "Missing Emacs dotfiles"
    
    [[ -e $emacs_file ]] && rm $emacs_file
    unlink_emacs_dir

    ln -s $dot_emacs_dir $emacs_dir
}

if [[ $1 == "-d" ]]; then
    unlink_emacs_dir
    exit 0
fi

requires_shell bash

requires emacs
requires_emacs_version "29.3"

requires sway
requires waybar

[[ $0 != $SHELL ]] && cd $(dirname $0)
link_emacs_dir
