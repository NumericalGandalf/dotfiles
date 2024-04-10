#!/usr/bin/sh

prepend_path () {
  case ":$PATH:" in
    *:"$1":*) ;;
    *) PATH="$1${PATH:+:${PATH}}"
  esac
}

prepend_ld_path () {
  case ":$LD_LIBRARY_PATH:" in
    *:"$1":*) ;;
    *) LD_LIBRARY_PATH="$1${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}"
  esac
}

prepend_c_path () {
  case ":$CPATH:" in
    *:"$1":*) ;;
    *) CPATH="$1${CPATH:+:${CPATH}}"
  esac
}

prepend_path "$HOME/.local/bin/"

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
prepend_ld_path "$HOME/.local/lib/"

export CC="clang"
export CPATH=$CPATH
prepend_c_path "$HOME/.local/include/"

export HISTSIZE=5000
export HISTIGNORE="*sudo -S*:$HISTIGNORE"

export TERMINAL="alacritty"
export EDITOR="emacsclient -c"
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
