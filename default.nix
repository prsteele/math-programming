{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, compiler ? "ghc902"
}:
let
  overrides = self: super: {
    glpk-headers           = self.callCabal2nix "glpk-headers"           ./glpk-headers          {};
    math-programming       = self.callCabal2nix "math-programming"       ./math-programming      {};
    math-programming-glpk  = self.callCabal2nix "math-programming-glpk"  ./math-programming-glpk {};
    math-programming-tests = self.callCabal2nix "math-programming-tests" ./math-programming-tests {};
  };
  hPkgs = pkgs.haskell.packages.${compiler}.override { inherit overrides; };
in
with hPkgs;
  { inherit
    glpk-headers
    math-programming
    math-programming-glpk
    math-programming-tests
    ;
  }
