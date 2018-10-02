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

simpleMIP :: (MonadIO m, IPMonad m Double) => m ()
simpleMIP = do
  x <- addVariable `asKind` Integer `within` Interval 0 5
  y <- addVariable `asKind` Continuous `within` Interval 0 5
  _ <- addConstraint (1 *: x .>= 1.1)
  _ <- addConstraint (1 *: y .>= 1.1)
  setObjective (1 *: x .+. 1 *: y)
  setSense Minimization
  _ <- optimizeLP
  status <- optimizeIP

  -- Check that we reached optimality
  liftIO $ status @?= Optimal

  vx <- evaluate x
  let xmsg = printf "Expected x to be 2, but is %.3f" vx
  liftIO $ assertBool xmsg (abs (vx - 2) <= 1e-1)

  vy <- evaluate y
  let ymsg = printf "Expected y to be 1.1, but is %.3f" vy
  liftIO $ assertBool ymsg (abs (vy - 1.1) <= 1e-1)

simpleMIPGlpk :: IO ()
simpleMIPGlpk = do
  result <- runGlpk simpleMIP
  case result of
    Left errorMsg -> assertFailure (show errorMsg)
    Right () -> return ()
  return ()
