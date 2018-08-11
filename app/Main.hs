module Main where

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

main :: IO ()
main = do
  print glpkFeasible
  print glpkInfeasible
  print glpkNoFeasible
  print glpkUnbounded
  print glpkUndefined
