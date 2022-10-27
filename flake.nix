{
  description = "Solving the Climate Crisis with Monads.";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: with inputs;
  flake-utils.lib.eachDefaultSystem (system:
    let
        pkgs = import nixpkgs {
          inherit system;
          # overlays = [];
        };

        myDevTools = [
          hPkgs.ghc
          stack-wrapped
          # External C libraries needed by some Haskell packages
          pkgs.zlib
        ];

        hPkgs = pkgs.haskell.packages."ghc902";

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
    in rec {
      devShell = pkgs.mkShell {
        buildInputs = [
          myDevTools
        ];

        NIX_PATH = "nixpkgs=" + pkgs.path;
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
      };
    }
  );
}
