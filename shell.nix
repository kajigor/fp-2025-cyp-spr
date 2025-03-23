{
  system ? builtins.currentSystem,
  pkgs ? import <nixpkgs> { inherit system; },
}:
pkgs.mkShell {
  packages = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.haskell-language-server
  ];
}
