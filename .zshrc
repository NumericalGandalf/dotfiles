#!/usr/bin/zsh

function pathappend() {
  for arg in "$@"; do
    if [ -d "$arg" ] && [[ ":$PATH:" != *":$arg:"* ]]; then
      PATH="${PATH:+"$PATH:"}$arg"
    fi
  done
}

function defaults() {
  export EDITOR=nvim
  export TERMINAL=kitty
  export BROWSER=chromium

  export HISTIGNORE="*sudo -S*:$HISTIGNORE"
}

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

function shorts() {
  alias ll="ls -lah"

  bindkey "^O" autosuggest-accept
  bindkey "^F" forward-word
  bindkey "^B" backward-word
  bindkey -r "^[F"
  bindkey -r "^[B"

  bindkey -r "^J"
  zle -N go-dir
  bindkey "^Ju" go-dir
  zle -N go-dir-editor
  bindkey "^Jn" go-dir-editor
}

function zrcmain() {
  pathappend $HOME/.local/bin $HOME/.spicetify
  defaults
  ohmyzsh
  shorts
}

zrcmain
