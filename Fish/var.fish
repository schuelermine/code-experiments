function var
    begin
        test "$argv[2]" = = && set -g $argv[1] $argv[3..-1]
    end || begin
        set -q argv[1] && not set -q argv[2] && echo $$argv[1]
    end
end
