{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Foreign.C.String
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

simple :: LPMonad m b => m (b, b, b)
simple = do
  x <- makeVariable
  setVariableBounds x NonNegativeReals
  y <- makeVariable `within` NonNegativeReals `asKind` Integer

  addConstraint $ 1 *: x .+. 1 *: y .>= 1
  addConstraint $ 1 *: y .-. 1 *: x .>= 1

  let objective = 1 *: x .+. 1 *: y

  setObjective objective
  setSense Minimization
  optimize

  xVal <- evaluateVariable x
  yVal <- evaluate y
  obj <- evaluate objective
  return (xVal, yVal, obj)

main :: IO ()
main = do
  problem <- glp_create_prob
  (xVal, yVal, obj) <- runReaderT (runGlpk simple) problem
  print (xVal, yVal, obj)
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
