#!/usr/bin/env nix-shell
#!nix-shell -i fish -p nixfmt
nixfmt ./**.nix
