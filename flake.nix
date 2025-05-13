{
  description = "Haskell environment with stack, ghcid, and HLS";

  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        ghc  = pkgs.haskell.compiler.ghc984;
      in {
        devShells.default = pkgs.mkShell {
	  NIX_PATH = "nixpkgs=${pkgs.path}";
          buildInputs = [
            pkgs.stack
            pkgs.haskell-language-server
            pkgs.ghcid
            ghc
          ];

          shellHook = ''
            echo "🟢 Haskell devShell active"
            echo "  → stack new myproj simple      # create project"
            echo "  → stack build / stack run      # build / run"
            echo "  → ghcid -c 'stack ghci'        # live recompile"
          '';
        };

        packages.default = pkgs.writeShellScriptBin "run" ''
          exec stack run "$@"
        '';
      });
}

