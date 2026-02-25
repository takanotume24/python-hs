{
  description = "python-hs development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            cabal-install
            haskell-language-server
            hlint
            ormolu
            pkg-config
            gmp
            zlib
            libffi
          ];

          shellHook = ''
            echo "python-hs nix dev shell"
            echo "- run: cabal update"
            echo "- run: cabal test"
            echo "- run: cabal run check-structure"
          '';
        };
      });
}
