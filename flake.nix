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
        pythonHsForChecks = haskellLib.addBuildTools pythonHs [ pkgs.cabal-install ];
        pythonHsWithChecks = haskellLib.doCheck pythonHsForChecks;
      in {
        checks = {
          cabal-test = pkgs.runCommand "python-hs-cabal-test" {
            nativeBuildInputs = [ pythonHsWithChecks ];
          } ''
            echo "cabal-test passed via doCheck: ${pythonHsWithChecks.name}"
            touch $out
          '';
          check-structure = pkgs.runCommand "python-hs-check-structure" {
            nativeBuildInputs = [ pythonHsWithChecks ];
          } ''
            check-structure
            touch $out
          '';
          check-runner-case-coverage = pkgs.runCommand "python-hs-check-runner-case-coverage" {
            nativeBuildInputs = [ pythonHsWithChecks ];
          } ''
            check-runner-case-coverage \
              ${./test/Test/Runner/RunnerEdgeSpec.hs} \
              ${./test/Test/Runner/RunnerVmParitySpec.hs} \
              ${./test/Test/VM/RunSourceVmSpec.hs}
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
            ncurses
            gmp
            zlib
            libffi
          ];

          shellHook = ''
            echo "python-hs nix dev shell"
            echo "- run: cabal update"
            echo "- run: cabal test"
            echo "- run: cabal run check-structure"
            echo "- run: cabal run check-runner-case-coverage -- test/Test/Runner/RunnerEdgeSpec.hs test/Test/Runner/RunnerVmParitySpec.hs test/Test/VM/RunSourceVmSpec.hs"
          '';
        };
      });
}
