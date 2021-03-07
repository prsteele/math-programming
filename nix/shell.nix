{ sources ? import ./sources.nix
, pkgs ? import sources.nixpkgs { }
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.haskell-language-server
  ];
}
