#!/usr/bin/sh

pgrep -x waybar > /dev/null && bar_up=1

if [[ "$1" == "-T" ]]
then
    if [[ -n "$bar_up" ]]
    then
        killall -SIGUSR2 waybar
    fi
else 
    if [[ -z "$bar_up" ]]
    then
	waybar &
	swaymsg smart_gaps off
    else
	killall waybar
	swaymsg smart_gaps on
    fi
fi
