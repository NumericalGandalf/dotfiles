#!/usr/bin/bash

path_prepend() {
    case ":$PATH:" in
        *:"$1":*) ;;
        *) PATH="$1${PATH:+:$PATH}"
    esac
}

path_prepend "$HOME/.local/bin/"

export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR=/usr/bin/emacs

export GTK_THEME="Adwaita:dark"

alias ls="ls --color=auto"
alias emacs="emacsclient -c"
