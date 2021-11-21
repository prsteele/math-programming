# Changelog for math-programming

## [0.4.0] -- 5 July 2020
### Added

- The `RealFrac` constraint on `LPMonad` numeric types.

  This simplifies the constraints necessary in application code.

### Removed

- The `writeFormulation` class method.

  This was impossible to implement correctly without requiring
  `LPMonad` to implement `MonadIO`, which should not be required in
  general.


## [0.3.0] -- 18 June 2020

Initial release.
