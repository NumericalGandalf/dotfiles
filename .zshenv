#!/usr/bin/zsh

function pathappend() {
  for arg in "$@"; do
    if [ -d "$arg" ] && [[ ":$PATH:" != *":$arg:"* ]]; then
      PATH="${PATH:+"$PATH:"}$arg"
    fi
  done
}

pathappend $HOME/.local/bin $HOME/.spicetify

export TERMINAL=alacritty
export EDITOR=nvim
export PAGER=less
export BROWSER=chromium

export HISTIGNORE="*sudo -S*:$HISTIGNORE"
