#!/usr/bin/sh

function info()
{
    echo "Info:" "$@"
}

function error()
{
    echo "Error:" "$@" >&2
    exit 1
}

function requires()
{
    local util=$(command -v $1)
    if [[ -z "$util" ]]
    then
	error "Requires utility" $1
    else
	info "Found utility" $util
    fi
}

function requires_shell()
{
    requires $1
    if [[ "$SHELL" != "/bin/${1}" ]]
    then
	error "Requires shell" $1
    fi
}

function requires_emacs_version()
{
    local version=$(emacs --version | head -n 1 | awk '{print $3}')
    if [[ $(printf '%s\n' $1 $version | sort -V | head -n1) != $1 ]]
    then
	error "Requires at least Emacs version" $1
    fi
}

function backup_file()
{
    [[ ! -e $1 ]] && return
    
    local d=$(dirname $1)
    local b=$(basename $1)
    local n=1

    function f()
    {
	echo "${d}/${b}.${n}.bak"
    }

    while [[ -e $(f) ]]
    do
	let n++
    done
    
    cp -r $1 $(f)
}

function link_emacs_dir()
{
    local emacs_file="$HOME/.emacs"
    local emacs_dir="$HOME/.emacs.d"
    backup_file $emacs_file
    backup_file $emacs_dir
    [[ -e $emacs_file ]] && rm $emacs_file
    [[ -e $emacs_dir ]] && rm $emacs_dir
    ln -sf $(realpath emacs) $emacs_dir
}

requires_shell bash

requires emacs
requires_emacs_version "29.3"

requires sway
requires waybar

cd $(dirname $0)
link_emacs_dir
