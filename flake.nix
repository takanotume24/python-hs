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
        # Keep the Nix toolchain aligned with cabal.project's `with-compiler`
        # and the GHC version used by CI (9.10.3).
        ghc = pkgs.haskell.compiler.ghc9103;
        packagesForGhc = pkgs.haskell.packages.ghc9103;
        haskellLanguageServer = packagesForGhc.haskell-language-server;
        pythonHs = packagesForGhc.callCabal2nix "python-hs" ./. { };
        pythonHsForChecks = haskellLib.addBuildTools pythonHs [ pkgs.cabal-install ];
        pythonHsWithChecks = haskellLib.doCheck pythonHsForChecks;
        devTools = with pkgs; [
          ghc
          cabal-install
          haskellLanguageServer
          hlint
          ormolu
          pkg-config
          ncurses
          gmp
          zlib
          libffi
          # CPython test integration (submodule: third_party/cpython)
          python3
          git
          gnumake
          openssl
          bzip2
          readline
          sqlite
        ];
        devToolsEnv = pkgs.buildEnv {
          name = "python-hs-dev-tools";
          paths = devTools;
          ignoreCollisions = true;
        };
      in {
        packages.default = devToolsEnv;

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
          packages = devTools;

          shellHook = ''
            echo "python-hs nix dev shell"
            echo "- run: cabal update"
            echo "- run: cabal test"
            echo "- run: cabal run check-structure"
            echo "- run: cabal run check-runner-case-coverage -- test/Test/Runner/RunnerEdgeSpec.hs test/Test/Runner/RunnerVmParitySpec.hs test/Test/VM/RunSourceVmSpec.hs"
            echo "- init CPython tests: git submodule update --init --recursive"
          '';
        };
      });
}
