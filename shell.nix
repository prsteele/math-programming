{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  local = import ./default.nix { inherit sources pkgs; };
in
pkgs.haskellPackages.shellFor {
  packages = p: [ local.glpk-headers
                  local.math-programming
                  local.math-programming-glpk
                  local.math-programming-tests
                ];
  buildInputs = [ pkgs.cabal-install
                  pkgs.cabal2nix
                  pkgs.haskell-language-server
                ];
  withHoogle = true;
}
