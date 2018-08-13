{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Foreign.C.String
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

simple = do
  x <- makeVariable
  setVariableBounds x NonNegativeReals
  y <- makeVariable `within` NonNegativeReals `asKind` Integer

  addConstraint $ 1 *: x .+. 1 *: y .>= 1
  addConstraint $ 1 *: y .-. 1 *: x .>= 1
  setObjective $ 1 *: x
  setSense Minimization
  xVal <- evaluateVariable x
  yVal <- evaluateVariable y
  return (xVal, yVal)

main :: IO ()
main = do
  problem <- glp_create_prob
  (xVal, yVal) <- runReaderT (runGlpk simple) problem
  print (xVal, yVal)
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
