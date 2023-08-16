{
  inputs.fenix.url = "github:nix-community/fenix";
  outputs = { self, nixpkgs, flake-utils, fenix }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        devShells.default = pkgs.stdenv.mkDerivation {
          name = "env";
          nativeBuildInputs = with pkgs; [
            fenix.packages.${system}.complete.toolchain
            bash
            clang
            gdb
            fish
            (ghc.withPackages
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
              ipython
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
