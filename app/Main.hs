{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad.Except
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
  z <- addVariable `named` "toDelete"

  x <- addVariable `named` "xvar" `within` NonNegativeReals
  y <- addVariable `named` "yvar" `within` NonNegativeReals `asKind` Integer

  c1 <- addConstraint (1 *: x .+. 1 *: y .>= 1) `named` "toDelete"
  c2 <- addConstraint (1 *: y .-. 1 *: x .>= 1) `named` "toBeRenamed"

  deleteVariable z
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

  cRef <- newIORef M.empty
  vRef <- newIORef M.empty

  let env = GlpkEnv problem cRef vRef

  result <- runReaderT (runExceptT (runGlpk simple)) env
  case result of
    Left error -> print error
    Right (xVal, yVal, obj) -> print (xVal, yVal, obj)
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
