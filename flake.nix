{
  description = "A collection of libraries for mathematical programming";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    systems.url = "github:nix-systems/default";
  };

  outputs = { self, nixpkgs, systems }:
    let
      forAllSystems = nixpkgs.lib.genAttrs (import systems);
      mkPkgs = system: import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = mkPkgs system;
        in
        { });

      devShells = forAllSystems (system:
        let
          pkgs = mkPkgs system;
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = hpkgs: with hpkgs; [
              examples
              glpk-headers
              math-programming
              math-programming-glpk
              math-programming-tests
            ];
            buildInputs = with pkgs; [
              haskellPackages.haskell-language-server
              ormolu
              cabal-install
            ];
            withHoogle = true;
          };
        });

      overlays = {
        default = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hfinal: hprev: {
              examples = hfinal.callCabal2nix "examples" ./examples { };
              glpk-headers = hfinal.callCabal2nix "glpk-headers" ./glpk-headers { };
              math-programming = hfinal.callCabal2nix "math-programming" ./math-programming { };
              math-programming-glpk = hfinal.callCabal2nix "math-programming-glpk" ./math-programming-glpk { };
              math-programming-tests = hfinal.callCabal2nix "math-programming-tests" ./math-programming-tests { };
            };
          };
        };
      };
    };
}
