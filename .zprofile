#!/usr/bin/zsh

function display() {
  termclt="$(tty | rev | cut -c 1)"
  if [[ $termclt -gt 3 ]]; then
    return
  fi

  if [[ -n $DISPLAY || -n $WAYLAND_DISPLAY ]]; then
    return
  fi

  xorg=1
  if [[ $xorg ]]; then
    session=i3
    if [[ $termclt -eq 2 ]]; then
      session=awesome
    fi
    if target=$(command -v $session); then
      exec startx $target
    fi
  fi
}

function zmain() {
  display
}

zmain
