# math-programming

This repository contains a number of Haskell packages used to solve
math programming problems. The
[`math-programming`](./math-programming/) package offers a generic
high-level interface for defining math programs, which backends like
[`math-programming-glpk`](./math-programming-glpk/) can use to compute
solutions. The [`glpk-headers`](./glpk-headers/) offers low-level
bindings the [GLPK](https://www.gnu.org/software/glpk/) solver.
