#!/usr/bin/sh

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

case "$(tty | rev | cut -c 1)" in
1) session=i3 ;;
2) session=awesome ;;
*) return ;;
esac

target=$(command -v $session) && exec $target
