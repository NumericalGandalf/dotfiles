#!/usr/bin/bash

path_prepend() {
    case ":$PATH:" in
        *:"$1":*) ;;
        *) PATH="$1${PATH:+:${PATH}}"
    esac
}

path_prepend "${HOME}/.local/bin/"

export ELECTRON_OZONE_PLATFORM_HINT=auto

export TERMINAL=alacritty
export EDITOR=nvim
export BROWSER=firefox

export GTK_THEME="Adwaita:dark"

alias ls="ls --color=auto"
