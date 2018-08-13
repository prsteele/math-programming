{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Foreign.C.String
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

simple :: LPMonad m Double => m ()
simple  = do
  x <- makeVariable
  y <- makeVariable
  setVariableBounds x NonNegative
  setVariableBounds y NonNegative
  addConstraint $ 1 *: x .+. 1 *: y .>= 1
  addConstraint $ 1 *: y .-. 1 *: x .>= 1
  setObjective $ 1 *: x
  setSense Minimization
  optimize
  return ()

main :: IO ()
main = do
  problem <- glp_create_prob
  runReaderT (runGlpk simple) problem
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
