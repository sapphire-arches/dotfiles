#!/bin/bash
xrdb -merge ~/.Xresources
xscreensaver -no-splash &
xcompmgr &
xsetroot -cursor_name left_ptr
hsetroot -solid '#4f4444'
