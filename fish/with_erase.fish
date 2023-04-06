function with_erase
    set -qgx $argv[1] || return -1
    set -l e $argv[1]
    set -e $argv[1]
    command $argv[2..-1]
    set -gx $argv[1] $e
end
