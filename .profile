BINX="$HOME/bin"
if [[ ":$PATH:" != *":$BINX:"* ]]; then
    export PATH="$PATH:$BINX"
fi

export TERMINAL="alacritty"

alias ll="ls -laFh -I. -I.."

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx &
fi
