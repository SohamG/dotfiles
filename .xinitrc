echo "Starting Xinitrc"


while :; do
    xrdb -load ~/.Xresources &
    nm-applet &
    dunst &
    dwmblocks &
    setxkbmap -option keypad:pointerkeys &
    setxkbmap -option caps:ctrl_modifier,shift:both_capslock

    dbus-launch /usr/local/bin/dwm 2> /dev/null

done

