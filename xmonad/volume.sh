volume_percent=$(amixer | grep -A 5 Master | grep "Mono:" | cut -d " " -f 6 | sed "s/\[//" | sed "s/\]//")
#strip leading spaces
echo -n "Volume:" $volume_percent
