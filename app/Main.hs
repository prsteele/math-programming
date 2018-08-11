module Main where

import Control.Monad.Reader
import Foreign.C.String
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk
import Math.Programming.Glpk.Header

tmp :: LPMonad m => m ()
tmp = do
  x <- makeVariable
  y <- makeVariable
  addConstraint $ Term x .+. Term y .>= 7
  return ()

main :: IO ()
main = do
  problem <- glp_create_prob
  runReaderT (runGlpk tmp) problem
  withCString "example.lp" (glp_write_lp problem nullPtr)
  return ()
