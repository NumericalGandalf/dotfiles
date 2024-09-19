#!/usr/bin/sh

. "${HOME}/.bashrc"

gentoo-pipewire-launcher &

if [[ "${XDG_VTNR}" = "1" ]]
then 
    export WLR_NO_HARDWARE_CURSORS=1
    exec sway
fi
