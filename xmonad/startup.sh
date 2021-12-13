#!/usr/bin/env bash

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

if [[ $(hostname) -eq "carrot-cake" ]]
then
  # carrot-cake only xrandr setup
  xrandr --output HDMI-0 --left-of DP-1
fi

fcitx &
xmodmap ~/.xmodmap
xrdb -merge ~/.Xresources
xscreensaver -no-splash &
xcompmgr &
xsetroot -cursor_name left_ptr
hsetroot -solid '#4f4444'
feh --bg-fill -z ~/Wallpapers/*
