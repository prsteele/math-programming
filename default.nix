{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  overrides = self: super: {
    examples               = self.callCabal2nix "examples"               ./examples               {};
    glpk-headers           = self.callCabal2nix "glpk-headers"           ./glpk-headers           {};
    math-programming       = self.callCabal2nix "math-programming"       ./math-programming       {};
    math-programming-glpk  = self.callCabal2nix "math-programming-glpk"  ./math-programming-glpk  {};
    math-programming-tests = self.callCabal2nix "math-programming-tests" ./math-programming-tests {};
  };
  hPkgs = pkgs.haskellPackages.override { inherit overrides; };
in
with hPkgs;
{ inherit
  examples
  glpk-headers
  math-programming
  math-programming-glpk
  math-programming-tests
  ;
}
