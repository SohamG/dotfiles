echo "Starting Xinitrc"


while :; do
    xrdb -load ~/.Xresources &
    nm-applet &
    dunst &
    dwmblocks &
    setxkbmap -option keypad:pointerkeys &

    ssh-agent dwm

done

