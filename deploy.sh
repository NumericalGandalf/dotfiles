#!/usr/bin/sh

[[ $0 != $SHELL ]] && cd $(dirname $0)

emacs_dir="$HOME/.emacs.d"

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

function require_shell()
{
    local req=$(cat assets/shell)
    require $req
    [[ "$SHELL" != "/bin/${req}" ]] && fail "Require shell" $req
}

function require_emacs()
{
    require emacs
    local req=$(cat assets/emacs_version)
    local version=$(emacs --version | head -n 1 | awk '{print $3}')
    [[ $(printf '%s\n' $req $version | sort -V | head -n1) != $req ]] &&
	fail "Require at least Emacs version" $req
}

function require_mandatories()
{
    cat assets/mandatories | while read -r line
    do
        require $line
    done
}

function unlink_emacs_dir()
{
    [[ -L $emacs_dir ]] && rm $emacs_dir
    [[ -d $emacs_dir ]] && rm -rf $emacs_dir
}

function link_emacs_dir()
{
    local emacs_file="$HOME/.emacs"
    local dot_emacs_dir=$(realpath emacs)
    [[ ! -d $dot_emacs_dir ]] && fail "Missing Emacs dotfiles"
    
    [[ -e $emacs_file ]] && rm $emacs_file
    unlink_emacs_dir

    ln -s $dot_emacs_dir $emacs_dir
}

if [[ $1 == "-d" ]]
then
    unlink_emacs_dir
    exit 0
fi

require_shell
require_emacs
require_mandatories

# link_emacs_dir
