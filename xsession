#!/bin/bash
function run_xmonad() {
#    trayer --edge bottom --align right --SetDockType true --SetPartialStrut true \
#     --expand true --width 10 --transparent true --tint 0x002b36 --height 12 &
    xscreensaver -no-splash &
    xsetroot -cursor_name left_ptr
    xbacklight -set =0
    xrdb -merge .Xresources
    orage &
#    wallpaper=$(ls /usr/local/share/backgrounds | sort -R | head -n1)
#    xloadimage -onroot "/usr/local/share/backgrounds/$wallpaper"
    xsetroot -solid rgb:93/a1/a1
#    xfce4-panel &
    xcompmgr -c &
    exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
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
