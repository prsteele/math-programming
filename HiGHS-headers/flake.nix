{
  description = "A HiGHS backend for math-programming";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        package = pkgs.callPackage ./highs.nix { };
      in
        {
          packages.HiGHS = package;
          packages.default = self.packages.${system}.HiGHS;
          devShells.default = pkgs.mkShell {
            buildInputs = [ pkgs.cmake package pkgs.gcc12 ];
          };
          devShell = self.devShells.${system}.default;
        }
    );
}
