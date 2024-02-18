#!/usr/bin/zsh

function display() {
  [[ -n "$DISPLAY" ]] && return

  case "$XDG_VTNR" in
  1) session=i3 ;;
  2) session=awesome ;;
  *) return ;;
  esac

  exec xinit $session
}

function zpmain() {
  display
}

zpmain
