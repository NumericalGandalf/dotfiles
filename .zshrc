export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="linuxonly"

ENABLE_CORRECTION="true"
COMPLETION_WAITING_DOTS="true"

plugins=(
    git 
    kubectl 
    history 
    emoji 
    encode64 
    jsontools 
    zsh-autosuggestions 
    web-search 
    sudo
)

source $ZSH/oh-my-zsh.sh
source ~/.profile
