{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            bash
            clang
            gdb
            fish
            haskell.packages.ghc924.ghc
            openjdk
            nix
            nushell
            powershell
            nodePackages.typescript
            nodejs
            python3
            python3Packages.autopep8
          ];
        };
      });
}
