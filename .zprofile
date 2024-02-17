#!/usr/bin/zsh

function display() {
  [[ -n $DISPLAY || -n $WAYLAND_DISPLAY ]] && return
  exec startx
}

function zpmain() {
  display
}

zpmain
