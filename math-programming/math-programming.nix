{ mkDerivation, base, containers, mtl, stdenv, tasty
, tasty-discover, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "math-programming";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [ base containers mtl text ];
  testHaskellDepends = [
    base tasty tasty-discover tasty-hunit tasty-quickcheck
  ];
  testToolDepends = [ tasty-discover ];
  description = "A library for formulating and solving math programs";
  license = stdenv.lib.licenses.bsd3;
}
