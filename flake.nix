{
  description = "A collection of libraries for mathematical programming";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    HiGHS-headers.url = "path:./HiGHS-headers";
  };

  outputs = { self, nixpkgs, flake-utils, HiGHS-headers, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        highs = pkgs.callPackage ./HiGHS-headers/highs.nix { };
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            examples               = self.callCabal2nix "examples"               ./examples               {};
            glpk-headers           = self.callCabal2nix "glpk-headers"           ./glpk-headers           {};
            math-programming       = self.callCabal2nix "math-programming"       ./math-programming       {};
            math-programming-glpk  = self.callCabal2nix "math-programming-glpk"  ./math-programming-glpk  {};
            math-programming-tests = self.callCabal2nix "math-programming-tests" ./math-programming-tests {};
          };
        };
      in
        {
          packages.examples = haskellPackages.examples;
          packages.glpk-headers = haskellPackages.glpk-headers;
          packages.math-programming = haskellPackages.mathProgramming;
          packages.math-programming-glpk = haskellPackages.mathProgrammingGlpk;
          packages.math-programming-tests = haskellPackages.mathProgrammingTests;
          packages.default = self.packages.${system}.examples;
          defaultPackage = self.packages.${system}.default;
          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ haskellPackages.examples
                            haskellPackages.glpk-headers
                            haskellPackages.math-programming
                            haskellPackages.math-programming-glpk
                            haskellPackages.math-programming-tests
                          ];
            buildInputs = [
              pkgs.haskellPackages.haskell-language-server
              pkgs.ormolu
              pkgs.cabal-install
              HiGHS-headers.packages.${system}.HiGHS
            ];
            withHoogle = true;
            doBenchmark = true;
          };
          devShell = self.devShells.${system}.default;
        }
    );
}
