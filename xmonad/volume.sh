#!/usr/bin/env bash

volume_percent=$(amixer -D pulse sget Master | grep "Front Left:" | cut -d " " -f 7 | sed "s/\[//" | sed "s/%\]//")
#strip leading spaces
echo -n "Volume:" $volume_percent
