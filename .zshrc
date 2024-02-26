#!/usr/bin/zsh

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="linuxonly"
plugins=(
  git
  zsh-autosuggestions
  zsh-syntax-highlighting
)
source $ZSH/oh-my-zsh.sh

alias ll="ls -lah"
alias fd="fd -H"
alias rg="rg --hidden -L -S -g '!.git/'"

bindkey -r "^[F"
bindkey -r "^[B"
bindkey -r "^J"

bindkey "^O" autosuggest-accept
bindkey "^F" forward-word
bindkey "^B" backward-word

zle -N go-dir
zle -N f-man

bindkey "^Jn" go-dir
bindkey "^Jm" f-man
