#!/usr/bin/bash

alias ls="ls --color=auto"

for path in "$HOME/.local/bin"; do
    case ":$PATH:" in
        *:"$path":*) ;;
        *) PATH="$path${PATH:+:$PATH}"
    esac
done

[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

[[ "$INSIDE_EMACS" = "vterm" ]] && . "$HOME/.config/emacs/vterm.sh"
