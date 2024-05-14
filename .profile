#!/usr/bin/sh

prepend_path () {
    case ":$PATH:" in
        *:"$1":*) ;;
        *) PATH="$1${PATH:+:${PATH}}"
    esac
}

prepend_path "$HOME/.local/bin/"

export HISTSIZE=5000
export HISTIGNORE="*sudo -S*:$HISTIGNORE"

export TERMINAL="alacritty"
export EDITOR="emacs"
export PAGER="less"
export BROWSER="firefox"

display () {
    case "$XDG_VTNR" in
        1) session=i3 ;;
        *) return ;;
    esac

    exec xinit $session
}

[[ -z "$DISPLAY" ]] && display
