echo "Starting Xinitrc"

    xrdb -load ~/.Xresources &
    nm-applet &
    dunst &
    dwmblocks &
    setxkbmap -option keypad:pointerkeys &
    setxkbmap -option caps:ctrl_modifier,shift:both_capslock &

    dbus-launch /usr/local/bin/dwm 2> /dev/null
#    polybar &
#    exec ssh-agent stumpwm

