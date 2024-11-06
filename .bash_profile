#!/usr/bin/bash 

[[ -f "$HOME/.bashrc" ]] && . "$HOME/.bashrc"

[[ -z "$WAYLAND_DISPLAY" ]] && [[ "$XDG_VTNR" = "1" ]] && exec sway --unsupported-gpu
