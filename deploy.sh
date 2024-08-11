#!/usr/bin/sh

[[ $0 != $SHELL ]] && cd $(dirname $0)

EMACS_DIR="$HOME/.emacs.d"
REQ_EMACS="29"

function fail()
{
    echo "Error:" "$@" >&2
    exit 1
}

function require()
{
    local util=$(command -v $1)
    [[ -z "$util" ]] && fail "Require utility" $1
}

function require_emacs()
{
    require emacs
    local version=$(emacs --version | head -n 1 | awk '{print $3}')
    [[ $(printf '%s\n' $REQ_EMACS $version | sort -V | head -n1) != $REQ_EMACS ]] &&
	fail "Require at least Emacs version" $REQ_EMACS
}

function unlink_emacs_dir()
{
    [[ -L $EMACS_DIR ]] && rm $EMACS_DIR
    [[ -d $EMACS_DIR ]] && rm -rf $EMACS_DIR
}

function link_emacs_dir()
{
    local emacs_dir=$(realpath emacs)
    [[ ! -d $emacs_dir ]] && fail "Missing Emacs dotfiles"
    
    [[ -e "$HOME/.emacs" ]] && rm "$HOME/.emacs"
    unlink_emacs_dir

    ln -s $emacs_dir $EMACS_DIR
}

if [[ $1 == "-d" ]]
then
    unlink_emacs_dir
    exit 0
fi

require_emacs
link_emacs_dir
