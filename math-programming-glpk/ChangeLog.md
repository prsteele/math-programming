# Changelog for `math-programming-glpk`

## [Unreleased]

### Changed

- Make the `Glpk` monad work with threaded runtimes. The underlying
  library requires that GLPK environments be accessed from a single
  thread, so we run the monad in bound threads.

## [0.4.1] -- 5 July 2020

### Fixed

- Infeasible problems now produce an `Infeasible` status rather than
  an `Error` status.

## [0.4.0] -- 5 July 2020

### Changed

- Updated to the `math-programming-0.4.0` API.

### Fixed

- How variable bounds are set.

  Free variables were being constrained to equal zero; they are now
  correctly set as free variables.

### Added

- The `writeFormulation` function.

  This replaces the `writeFormulation` class method.

### Removed

- The `writeFormulation` class method.

  This is consistent with the `math-programming-0.4.0` API.

## [0.3.0] -- 18 June 2020

Initial release.
