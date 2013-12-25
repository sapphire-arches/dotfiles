volume_percent=$(amixer | grep -A 5 Master | grep "Front Left:" | cut -d " " -f 7 | sed "s/\[//" | sed "s/\]//")
#strip leading spaces
echo -n "Volume:" $volume_percent
