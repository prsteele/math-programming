{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
}:
let
  extraDeps = [
    pkgs.cabal-install
    pkgs.haskellPackages.haskell-language-server
  ];
in
import ../shell.nix { inherit sources pkgs compiler extraDeps; }
