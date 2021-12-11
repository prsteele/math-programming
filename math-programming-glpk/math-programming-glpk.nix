{ mkDerivation, base, glpk-headers, math-programming-tests, stdenv
, tasty, tasty-discover, tasty-hunit
}:
mkDerivation {
  pname = "math-programming";
  version = "0.5.0";
  src = ./.;
  libraryHaskellDepends = [ base glpk-headers ];
  testHaskellDepends = [
    base math-programming-tests tasty tasty-discover tasty-hunit
  ];
  testToolDepends = [ tasty-discover ];
  description = "A GLPK backend to the math-programming library";
  license = stdenv.lib.licenses.bsd3;
}
