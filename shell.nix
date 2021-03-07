{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc884"
, extraDeps ? [ pkgs.cabal-install ]
}:
(import ./default.nix { inherit sources pkgs compiler extraDeps; }).env
