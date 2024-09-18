#!/usr/bin/sh

. "$HOME/.bashrc"

gentoo-pipewire-launcher &

export WLR_NO_HARDWARE_CURSORS=1

[[ -z "$WAYLAND_DISPLAY" ]] && [[ "$XDG_VTNR" -eq "1" ]] && exec sway
