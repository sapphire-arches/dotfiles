#!/bin/bash
function run_xmonad() {
    xscreensaver -no-splash &
    xsetroot -cursor_name left_ptr
#    xbacklight -set =0
#    xrdb -merge .Xresources
#    orage &
#    xsetroot -solid rgb:93/a1/a1
    xsetroot -solid rgb:4f/44/44
#    xcompmgr -c &
    exec ck-launch-session dbus-launch --sh-syntax --exit-with-session xmonad
}

run_xmonad

# DEFAULT_SESSION=xmonad
# case $1 in
# xmonad)
#     run_xmonad
#     ;;
# xfce4)
#     exec ck-launch-session xfce4-session
#     ;;
# gnome)
#     exec gnome-session
#     ;;
# kde)
#     exec ck-launch-session startkde
#     ;;
# *)
#     run_xmonad
#     ;;
# esac
