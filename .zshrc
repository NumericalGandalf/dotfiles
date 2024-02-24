#!/usr/bin/zsh

function {
  export ZSH="$HOME/.oh-my-zsh"
  ZSH_THEME="linuxonly"
  plugins=(
    git
    zsh-autosuggestions
    zsh-syntax-highlighting
  )
  source $ZSH/oh-my-zsh.sh
}

function {
  alias ll="ls -lah"
  alias fd="fd -H"
  alias rg="rg --hidden -L -S -g '!.git/'"
}

function go-dir() {
  local old=$(pwd) && cd $HOME && local target=$(
    fd --strip-cwd-prefix -t d . | fzf -i --preview "ls -lah {}"
  ) && cd $target && zle reset-prompt && $EDITOR . || cd $old
}

function f-man() {
  local target=$(man -k . | cut -d " " -f 1-2 |
    fzf -i --preview "echo {1} | xargs whatis") || return
  local page=$(echo $target | cut -d " " -f 2 |
    awk -F'[)(]' '{print $2}')
  man $(echo $target | cut -d " " -f 1).$page
}

function {
  bindkey -r "^[F"
  bindkey -r "^[B"
  bindkey -r "^J"

  bindkey "^O" autosuggest-accept
  bindkey "^F" forward-word
  bindkey "^B" backward-word

  zle -N go-dir
  zle -N f-man
  zle -N f-bg
  zle -N re-rc

  bindkey "^Jn" go-dir
  bindkey "^Jm" f-man
}
