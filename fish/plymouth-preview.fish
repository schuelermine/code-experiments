#!/usr/bin/env fish
if test "$USER" != root
    sudo -- (status filename) $argv
    exit
end
if not test "$argv[1]" -lt 80
    if test "$status" = 2
        echo "Duration must be less than 80 seconds" >&2
        exit 1
    else
        echo "Duration must be a number" >&2
        exit 2
    end
end
if not set -q argv[2] || not test "$argv[2]" -lt 32
    set argv[2] 1
end
set dt (math 1 / "$argv[2]")
set steps (math "$argv[1]" \* "$argv[2]")
plymouthd
plymouth show-splash
plymouth change-mode --updates
for step in (seq "$steps")
    set percentage (math --scale=0 100 \* "$step" / "$steps")
    plymouth system-update --progress="$percentage"
    echo "$percentage% done"
    sleep "$dt"
end
echo "Done!"
plymouth quit
