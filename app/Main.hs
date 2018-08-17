{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Foreign.C.String
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

simple :: LPMonad m b => m (b, b, b)
simple = do
  x <- makeVariable `named` "xvar"
  setVariableBounds x NonNegativeReals
  y <- makeVariable `within` NonNegativeReals `asKind` Integer

  nameVariable y "yvar"

  c1 <- addConstraint $ 1 *: x .+. 1 *: y .>= 1
  c2 <- addConstraint $ 1 *: y .-. 1 *: x .>= 1

  deleteConstraint c1

  nameConstraint c2 "con2"

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

  ref <- newIORef M.empty

  let env = GlpkEnv problem ref

  (xVal, yVal, obj) <- runReaderT (runGlpk simple) env
  print (xVal, yVal, obj)
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
