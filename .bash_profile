#!/usr/bin/bash 

[[ -f "$HOME/.bashrc" ]] && . "$HOME/.bashrc"

[[ "$XDG_VTNR" = "1" ]] && exec sway --unsupported-gpu
