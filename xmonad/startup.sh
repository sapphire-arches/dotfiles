#!/usr/bin/env bash

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

fcitx &
xmodmap ~/.xmodmap
xrdb -merge ~/.Xresources
xscreensaver -no-splash &
xcompmgr &
xsetroot -cursor_name left_ptr
hsetroot -solid '#4f4444'
feh --bg-fill -z ~/Wallpapers/*
