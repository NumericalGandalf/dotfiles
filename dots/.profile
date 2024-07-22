#!/usr/bin/sh

export PATH="$HOME/.local/bin:$PATH"

export WLR_NO_HARDWARE_CURSORS=1

gentoo-pipewire-launcher &

[[ -z "$WAYLAND_DISPLAY" ]] && [[ "$XDG_VTNR" -eq "1" ]] && exec sway
