#!/usr/bin/sh

prepend_path () {
  case ":$PATH:" in
    *:"$1":*) ;;
    *) PATH="$1${PATH:+:${PATH}}"
  esac
}

prepend_path "$HOME/.local/bin/"

export HISTSIZE=5000
export HISTIGNORE="*sudo -S*:$HISTIGNORE"

export TERMINAL="alacritty"
export EDITOR="emacs"
export ALTERNATE_EDITOR="emacs"
export PAGER="less"
export BROWSER="brave"

export XDG_CONFIG_HOME="$HOME/.config/"

export WLR_NO_HARDWARE_CURSORS=1

display () {
  case "$XDG_VTNR" in
    1) session=i3 ;;
    2) exec sway ;;
    *) return ;;
  esac

  exec xinit $session
}

[[ -z "$DISPLAY" ]] && [[ -z "$WAYLAND_DISPLAY" ]] && display
