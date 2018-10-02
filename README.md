# math-programming

A math programming library.

This library is designed to formulate and solve math programs, in
particular linear programs and mixed-integer linear programs.

This library alone is not sufficient to solve math programs; to do so,
a solver backend implementing the `LPMonad` or `IPMonad` classes is
required, such as the [GLPK
backend](https://github.com/prsteele/math-programming-glpk).
