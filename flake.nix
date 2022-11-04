{
  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          bash
          clang
          fish
          ghc
          openjdk
          nix
          nushell
          powershell
          nodePackages.typescript
          nodejs
        ];
      };
    });
}