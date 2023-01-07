{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import Math.Programming
import Math.Programming.Glpk
import Text.Printf

main :: IO ()
main = runGlpk $ do
  x <- free `named` "x"
  y <- bounded 2 5 `named` "y"
  z <- nonNeg `named` "z"

  c1 <- 1 .* x .+ 1 .* y .+ 2 .* z .==# 10
  c2 <- 3 .* x .+ 2 .* y .+ 1 .* z .==# 5

  let objExpr = vsum [x, y, z]
  obj <- minimize objExpr

  objExprS <- formatExpr objExpr
  liftIO . TIO.putStrLn $ "min " <> objExprS

  let displayExpr :: Expr GlpkVariable -> Glpk ()
      displayExpr = formatExpr >=> liftIO . TIO.putStrLn

  liftIO . putStrLn $ "min "

  status <- optimizeLP
  liftIO . putStrLn $ "the solution was found to be " <> show status

  optValue <- getObjectiveValue obj
  liftIO . putStrLn $ printf "objective value = %f" optValue

  let displayVar v = do
        name <- getName v
        value <- getVariableValue v
        liftIO . putStrLn $ printf "%s = %f" name value

  liftIO . putStrLn $ "variables"
  displayVar x
  displayVar y
  displayVar z

  let displayDual c = do
        name <- getName c
        value <- getConstraintValue c
        liftIO . putStrLn $ printf "%s = %f" name value

  liftIO . putStrLn $ "constraints"
  displayDual c1
  displayDual c2
