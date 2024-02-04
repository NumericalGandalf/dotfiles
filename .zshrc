#!/usr/bin/zsh

ohmyzsh() {
  export ZSH="$HOME/.oh-my-zsh"
  ZSH_THEME="linuxonly"
  plugins=(
    git 
    kubectl 
    emoji 
    encode64 
    jsontools 
    zsh-autosuggestions 
    web-search 
    zsh-syntax-highlighting
  )

  source $ZSH/oh-my-zsh.sh
}

zshmain() {
  source ~/.profile
  ohmyzsh
  pathappend $HOME/.spicetify
}

zshmain
