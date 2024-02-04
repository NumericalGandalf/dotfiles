source ~/.profile

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

export PATH=$PATH:/home/gandalf/.spicetify
