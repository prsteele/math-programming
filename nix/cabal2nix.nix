{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
}:
pkgs.mkShell { buildInputs = [pkgs.cabal2nix]; }
