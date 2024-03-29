# math-programming

This repository contains a number of Haskell packages used to solve
math programming problems.

## Quick start

Check out the [`examples`](./examples/); these show you how to get
started modeling and solving math programs.

## Packages

- [`math-programming`](./math-programming/), a high-level interface
  for defining math programs.
- [`math-programming-glpk`](./math-programming-glpk/), a solver
  backend for [`math-programming`](./math-programming/) interfacing
  with the [GLPK](https://www.gnu.org/software/glpk/) solver.
- [`math-programming-tests`](./math-programming-tests/), helper
  functions for defining tests for new solver backends.
- [`glpk-headers`](./glpk-headers/), low-level bindings the
  [GLPK](https://www.gnu.org/software/glpk/) solver.
- [`examples`](./examples/), a collection of example code.

## Development

The easiest way to work on this project is to use the [Nix package
manager](https://nixos.org/); with `nix` installed, running

```
nix-shell
```

in the root directory will put you a development shell with all the
dependencies necessary for a traditional `cabal`-based development
cycle.
