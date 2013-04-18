function run_xmonad() {
#    trayer --edge bottom --align right --SetDockType true --SetPartialStrut true \
#     --expand true --width 10 --transparent true --tint 0x002b36 --height 12 &
    xscreensaver -no-splash &
    pulseaudio --start
    xsetroot -cursor_name left_ptr
    xbacklight -set =0
    xrdb -merge .Xresources
    orage &
    xloadimage -onroot /usr/local/share/backgrounds/wallpaper.png
    xsetroot -solid rgb:00/2b/36
    xfce4-panel &
    exec ck-launch-session xmonad
} 

DEFAULT_SESSION=xmonad
case $1 in
xmonad)
    run_xmonad
    ;;
xfce4)
    exec ck-launch-session xfce4-session
    ;;
gnome)
    exec gnome-session
    ;;
*)
    run_xmonad
    ;;
esac
