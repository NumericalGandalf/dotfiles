#!/usr/bin/sh 

has_comma=0
i3status | (read line && echo $line && read line && echo $line && while : 
do
    read line
    [ $has_comma -eq 1 ] && line="${line#?}"

    line=$(jq -Mc '[.[] | select(.full_text != "Eth: down")]' <<< "$line")
    line=$(jq -Mc '[.[] | select(.full_text != "Wifi: down")]' <<< "$line")
        
    kb=$(swaymsg -t get_inputs | jq -cMr 'map(select(.type == "keyboard")) | .[0].xkb_active_layout_name')
    line=$(echo $line | sed "s/KEYBOARD/Kb: $kb/")
    
    [ $has_comma -eq 1 ] && line=",${line}" || has_comma=1

    echo $line
done)
