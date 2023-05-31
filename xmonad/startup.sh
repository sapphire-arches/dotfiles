#!/usr/bin/env bash

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS="@im=fcitx"

hostname=$(hostname)

start_if_not_running() {
  if ! (ps -ax | grep $1 | grep -v grep > /dev/null)
  then
    $@ &
  fi
}

case $(hostname) in
  "carrot-cake")
    # carrot-cake only xrandr setup
    xrandr --output HDMI-0 --left-of DP-1
    ;;
  "smith-island")
    start_if_not_running trayer --height 26 --heighttype pixel --edge bottom --widthtype request --align right &
    ;;
  *)
    ;;
esac

fcitx &
xmodmap ~/.xmodmap
xrdb -merge ~/.Xresources
xsetroot -cursor_name left_ptr
feh --bg-fill -z ~/Wallpapers/*
start_if_not_running xscreensaver -no-splash &
start_if_not_running xcompmgr &
start_if_not_running obsidian &

eww -c ~/.config/eww/bar open bar
eww -c ~/.config/eww/bar update visible=true
