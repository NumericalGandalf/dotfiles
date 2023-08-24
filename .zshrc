export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="lukerandall"

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(git)

source $ZSH/oh-my-zsh.sh
source ~/.profile

alias ll="ls -laFh -I. -I.."
