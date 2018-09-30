{-# LANGUAGE FlexibleContexts #-}
module SimpleMIP where

import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf

import Math.Programming
import Math.Programming.Glpk

test_simple :: TestTree
test_simple = testGroup "Simple MIP problems"
  [ testCase "Simple MIP (GLPK)" simpleMIPGlpk
  ]

--simpleMIP :: (MonadIO m, LPMonad m Double) => m ()
simpleMIP :: Glpk ()
simpleMIP = do
  x <- addVariable `asKind` Integer `within` Interval 0 5
  _ <- addConstraint (1 *: x .>= 1.1)
  setObjective (1 *: x)
  setSense Minimization
  _ <- optimizeLP
  status <- optimize

  -- Check that we reached optimality
  liftIO $ status @?= Optimal

  v <- evaluate x
  let msg = printf "Expected x to be 2, but is %.3f" v
  writeFormulation "simplemip.lp"
  liftIO $ assertBool msg (abs (v - 2) <= 1e-1)

simpleMIPGlpk :: IO ()
simpleMIPGlpk = do
  result <- runGlpk simpleMIP
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()
