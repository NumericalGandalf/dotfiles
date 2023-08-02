export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="jispwoso"

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH="$PATH:$HOME/.bin"

alias ll="ls -laFh -I. -I.."
