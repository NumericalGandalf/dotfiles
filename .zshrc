#!/usr/bin/zsh

function ohmyzsh() {
  export ZSH="$HOME/.oh-my-zsh"
  ZSH_THEME="gentoo"
  plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
  )
  source $ZSH/oh-my-zsh.sh
}

function shorts() {
  alias ll="ls -lah"
}

function go-dir() {
  target=$(fd -H . -t d $HOME | fzf -i --preview "ls -lah {}") &&
    cd $target && zle reset-prompt
}

function go-dir-editor() {
  go-dir && $EDITOR .
}

function f-man() {
  target=$(man -k . | cut -d " " -f 1 | uniq | fzf -i) && man $target
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
