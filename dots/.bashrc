#!/usr/bin/sh

function path_prepend ()
{
    if [[ -d "$1" ]]
    then 
        case ":$PATH:" in
            *:"$1":*) ;;
            *) PATH="$1${PATH:+:${PATH}}"
        esac
    fi
}

path_prepend "${HOME}/.local/bin/"

export EDITOR="emacsclient -c"
export ALTERNATE_EDITOR="emacs"

export BROWSER="firefox"

if [[ "${INSIDE_EMACS}" -eq "vterm" ]]
then
    . "${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh"
fi
