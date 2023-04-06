#!/usr/bin/env nix-shell
#!nix-shell -i fish -p clang-tools
clang-format -i ./**.c
