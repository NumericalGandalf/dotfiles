#!/usr/bin/zsh

pathappend() {
  for ARG in "$@"; do
    if [ -d "$ARG" ] && [[ ":$PATH:" != *":$ARG:"* ]]; then
      PATH="${PATH:+"$PATH:"}$ARG"
    fi
  done
}

defaults() {
  export EDITOR=nvim
  export TERMINAL=kitty
  export BROWSER=chromium
}

ohmyzsh() {
  export ZSH="$HOME/.oh-my-zsh"
  ZSH_THEME="linuxonly"
  plugins=(
    git
    kubectl
    emoji
    encode64
    history
    jsontools
    zsh-autosuggestions
    web-search
    zsh-syntax-highlighting
  )

  source $ZSH/oh-my-zsh.sh
}

zrcmain() {
  pathappend $HOME/.local/bin $HOME/.spicetify
  defaults
  ohmyzsh
}

zrcmain
