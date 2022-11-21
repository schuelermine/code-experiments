function ...
    switch (count $argv)
        case 0
            set c 2
        case 1
            set c $argv[1]
        case "*"
            echo "... takes up to one argument, but $(count $argv) were given" >&2
    end
    set dest $PWD
    for i in (seq $c)
        set dest (dirname $dest)
    end
    if isatty stdout
        cd $dest
    else
        echo $dest
    end
end

