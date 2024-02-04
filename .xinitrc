. /etc/X11/xinit/xinitrc-common

if [ -f $HOME/.Xclients ]; then
	exec $CK_XINIT_SESSION $SSH_AGENT $HOME/.Xclients ||
		exec $CK_XINIT_SESSION $SSH_AGENT $HOME/.Xclients
elif [ -f /etc/X11/xinit/Xclients ]; then
	exec $CK_XINIT_SESSION $SSH_AGENT /etc/X11/xinit/Xclients ||
		exec $CK_XINIT_SESSION $SSH_AGENT /etc/X11/xinit/Xclients
else
	[ -x /usr/bin/xsetroot ] && /usr/bin/xsetroot -solid '#222E45'
	[ -x /usr/bin/xclock ] && /usr/bin/xclock -geometry 100x100-5+5 &
	[ -x /usr/bin/xterm ] && xterm -geometry 80x50-50+150 &
	[ -x /usr/bin/twm ] && /usr/bin/twm
fi

session=${1:-i3}
case $session in
kde) exec startplasma-x11 ;;
gnome) exec gnome-session ;;
*) exec $1 ;;
esac
