let nixpkgs = builtins.getFlake "nixpkgs";
pkgs = nixpkgs.legacyPackages.x86_64-linux;
in pkgs.coreutils.overrideAttrs ({...}: { NIX_DEBUG = 7; builder = ""; })
