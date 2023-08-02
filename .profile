BINX="$HOME/bin"
if [[ ":$PATH:" != *":$BINX:"* ]]; then
    export PATH="$PATH:$BINX"
fi

alias ll="ls -laFh -I. -I.."
