# You can install extra packages in the shell environment by
# specifying additional arguments to extraDeps
{
  sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
, extraDeps ? []
}:
let
  mkDerivation =
    args@{ librarySystemDepends ? [], ... }:
    pkgs.haskell.packages.${compiler}.mkDerivation
      ( args // { librarySystemDepends = librarySystemDepends ++ extraDeps; });
in
pkgs.haskell.packages.${compiler}.callPackage ./math-programming-glpk.nix { inherit mkDerivation; }
