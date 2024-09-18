#!/usr/bin/sh

. "$HOME/.bashrc"

gentoo-pipewire-launcher &

if [[ -z "${WAYLAND_DISPLAY}" ]]
then 
    export WLR_NO_HARDWARE_CURSORS=1
    exec sway
fi