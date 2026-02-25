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
        haskellLib = pkgs.haskell.lib;
        pythonHs = pkgs.haskellPackages.callCabal2nix "python-hs" ./. { };
        pythonHsWithChecks = haskellLib.doCheck pythonHs;
      in {
        checks = {
          cabal-test = pkgs.runCommand "python-hs-cabal-test" {
            nativeBuildInputs = [ pythonHsWithChecks ];
          } ''
            echo "cabal-test passed via doCheck: ${pythonHsWithChecks.name}"
            touch $out
          '';
          check-structure = pkgs.runCommand "python-hs-check-structure" {
            nativeBuildInputs = [ pythonHs ];
          } ''
            check-structure
            touch $out
          '';
        };

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
