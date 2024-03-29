#!/usr/bin/sh

append_path() {
  case ":$PATH:" in
    *:"$1":*) ;;
    *) PATH="${PATH:+$PATH:}$1"
  esac
}

append_ld_path() {
  case ":$LD_LIBRARY_PATH:" in
    *:"$1":*) ;;
    *) LD_LIBRARY_PATH="${LD_LIBRARY_PATH:+$LD_LIBRARY_PATH:}$1"
  esac
}

append_path "$HOME/.local/bin/"

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH
append_ld_path "$HOME/.local/lib/"

export BASH_ENV="$HOME/.bash_env"

export HISTSIZE=5000
export HISTIGNORE="*sudo -S*:$HISTIGNORE"
export SUDO_ASKPASS="$HOME/.local/bin/askpass"

export TERMINAL=alacritty
export EDITOR=vim
export ALTERNATE_EDITOR=emacs
export PAGER=less
export BROWSER=chromium
export CC=clang

export GTK_THEME=Adwaita:dark

display() {
  case "$XDG_VTNR" in
  1) session=i3 ;;
  2) session=awesome ;;
  3) session=dwm ;;
  *) return ;;
  esac
  
  exec xinit $session
}

[[ -z "$DISPLAY" ]] && display
