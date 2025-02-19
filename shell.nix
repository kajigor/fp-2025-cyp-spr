{
  system ? builtins.currentSystem,
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { inherit system; },
}:
pkgs.mkShell {
  packages = [
    pkgs.cabal-install
    pkgs.ghc
    pkgs.haskell-language-server
  ];
}
