#!/usr/bin/sh

export PATH="$HOME/.local/bin:$PATH"

export BROWSER="firefox"

export WLR_NO_HARDWARE_CURSORS=1

gentoo-pipewire-launcher restart &

[[ -z "$WAYLAND_DISPLAY" ]] && [[ "$XDG_VTNR" -eq "1" ]] && exec sway
