#!/usr/bin/zsh

function ohmyzsh() {
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

function shorts() {
  alias ll="ls -lah"
}

function go-dir() {
  if target=$(fd -H . -t d $HOME | fzf -i --preview "ls -lah {}"); then
    cd $target
    zle accept-line
    return 0
  fi
  return 1
}

function go-dir-editor() {
  if go-dir; then
    $EDITOR .
  fi
}

function f-man() {
  if target=$(man -k . | cut -d " " -f 1 | uniq | fzf -i); then
    man $target
  fi
}

function keybinds() {
  bindkey "^O" autosuggest-accept
  bindkey "^F" forward-word
  bindkey "^B" backward-word
  bindkey -r "^[F"
  bindkey -r "^[B"

  zle -N go-dir
  zle -N go-dir-editor
  zle -N f-man

  bindkey -r "^J"
  bindkey "^Ju" go-dir
  bindkey "^Jn" go-dir-editor
  bindkey "^Jm" f-man
}

function zrcmain() {
  ohmyzsh
  shorts
  keybinds
}

zrcmain
