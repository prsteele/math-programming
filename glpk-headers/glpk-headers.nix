{ mkDerivation, base, derive-storable, glpk, lib, tasty
, tasty-discover, tasty-hunit
}:
mkDerivation {
  pname = "glpk-headers-haskell";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [ base derive-storable ];
  librarySystemDepends = [ glpk ];
  testHaskellDepends = [ base tasty tasty-discover tasty-hunit ];
  testSystemDepends = [ glpk ];
  testToolDepends = [ tasty-discover ];
  description = "Low-level Haskell bindings to the GLPK library";
  license = lib.licenses.bsd3;
}
