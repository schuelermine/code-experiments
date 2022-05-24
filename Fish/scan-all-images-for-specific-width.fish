#!/usr/bin/env fish
set min $argv[1]
set max $argv[2]
set dim $argv[3]
set outfile $argv[1]
function img-dimensions
    set filename "$argv[1]"
    set output (identify $filename 2>/dev/null) || begin
        set magick_status $status
        echo "Error while reading or decoding file $filename" >&2
        return $magick_status
    end
    set info (string sub --start (math 2 + (string length $filename)) $output)
    echo -- $info | sed -E 's/^(\w+) ([0-9]+)x([0-9]+) ([0-9.]+)x([0-9.]+)\+([0-9.]+)\+([0-9.]+) (.+) ([0-9.]+)([KMGTEPZ]i?)?B ([0-9.]+)u (.+)$/\2\n\3/'
end
set files $argv[4..-1]
set count (count $files)
set index 0
set -g errtime 0
set -g msg ""
function echo-1
    echo -n $argv | string collect -N
end
function get-status
    if test "$argv[1]" = E
        set errtime (date +%s%3N)
        echo-1 "[E] "
    else if test (math (date +%s%3N) - "$errtime") -le 500
        echo-1 -s "[E] " (echo-1 $argv[2..-1])
    else
        echo-1 -s "[$argv[1]] " (echo-1 $argv[2..-1])
    end
end
function out-msg
    set text (echo-1 $argv)
    echo -n -- $text >&2
    set msg "$msg"(echo -n -- $text)[-1]
end
function clear-msg
    for i in (seq (math $COLUMNS - (string length -- "$msg")))
        echo -n ' ' >&2
    end
    echo -n \r >&2
    set msg ""
    if test (count $argv) -gt 0
        out-msg $argv
    end
end
out-msg "[ ] "
for file in $files
    set index (math "$index" + 1)
    out-msg "Reading $file ($index/$count)…"
    set dimensions (img-dimensions $file 2>/dev/null) || begin
        clear-msg (get-status E)
        continue
    end
    if test "$dimensions[$dim]" -le $max -a "$dimensions[$dim]" -ge $min 2>/dev/null
        clear-msg
        echo -- $file >> $outfile
        out-msg (get-status Y -s "$file" \n | string collect -N)
    else
        if test "$status" = 2
            clear-msg (get-status E)
            continue
        end
        clear-msg (get-status N)
    end
end
