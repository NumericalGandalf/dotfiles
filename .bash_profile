#!/usr/bin/bash 

[[ -f "${HOME}/.bashrc" ]] && . "${HOME}/.bashrc"

if [[ "${XDG_VTNR}" = "1" ]]; then 
    exec sway --unsupported-gpu
fi
