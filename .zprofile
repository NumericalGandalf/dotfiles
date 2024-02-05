#!/usr/bin/zsh

display() {
  if [[ "$(tty | rev | cut -c 1)" -gt 3 ]]; then
    return 0
  fi

  xorg=1

  if [[ $xorg ]]; then
    export XDG_SESSION_TYPE=x11
  else
    export XDG_SESSION_TYPE=wayland
  fi

  if [[ -n $DISPLAY || -n $WAYLAND_DISPLAY ]]; then
    return 0
  fi

  if [[ $xorg ]]; then
    exec startx 
  fi

  export XDG_SESSION_TYPE=tty
}

zmain() {
  neofetch
  display
}

zmain
