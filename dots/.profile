#!/usr/bin/bash

. "${HOME}/.bashrc"

if [[ "${XDG_VTNR}" = "1" ]]; then 
    export WLR_NO_HARDWARE_CURSORS=1
    exec sway
fi
