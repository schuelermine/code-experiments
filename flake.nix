{
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.stdenv.mkDerivation {
          name = "env";
          nativeBuildInputs = with pkgs; [
            bash
            clang
            gdb
            fish
            (haskell.packages.ghc924.ghc.withPackages
              (hspkgs: with hspkgs; [ QuickCheck ]))
            openjdk
            nix
            nushell
            powershell
            nodePackages.typescript
            nodejs
            (python3.withPackages (pypkgs: with pypkgs; [
              hypothesis
              pytest
            ]))
            mypy
            black
            nixfmt
            clang-tools
            ormolu
            cargo
            rustc
            rustfmt
            clippy
            bear
          ];
          buildInputs = with pkgs; [
            mpfr
          ];
        };
      });
}
