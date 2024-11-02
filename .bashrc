#!/usr/bin/bash

export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR=/usr/bin/emacs
export TERMINAL=alacritty
export BROWSER=firefox

alias emacs="emacsclient -c"
alias ls="ls --color=auto"

for path in "$HOME/.local/bin"; do
    case ":$PATH:" in
        *:"$path":*) ;;
        *) PATH="$path${PATH:+:$PATH}"
    esac
done

[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"
