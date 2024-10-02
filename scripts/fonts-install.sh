#!/usr/bin/sh

fonts_dir="$HOME/.local/share/fonts/"
[[ ! -d $fonts_dir ]] && mkdir -p $fonts_dir

cp *.ttf $fonts_dir
fc-cache -f
