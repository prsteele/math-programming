{-# LANGUAGE FlexibleContexts #-}
module Math.Programming.Tests.IP where

import           Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Math.Programming

makeIPTests
  :: (PrintfArg (Numeric m), RealFrac (Numeric m), MonadIO m, IPMonad m)
  => (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeIPTests runner = testGroup "IP problems"
  [ testCase "Simple MIP" (runner simpleMIPTest)
  ]

-- | We solve a simple MIP of the form
--
-- @
-- min  x + y
-- s.t. x >= 1.1
--      y >= 1.1
--      0 <= x <= 5
--      0 <= y <= 5
--      x integer
-- @
--
-- The optimal solution to this MIP is x = 2, y = 1.1.
simpleMIPTest :: (PrintfArg (Numeric m), RealFrac (Numeric m), MonadIO m, IPMonad m) => m ()
simpleMIPTest = do
  x <- bounded 0 5 `asKind` Integer
  y <- bounded 0 5 `asKind` Continuous
  _ <- x @>=# 1.1
  _ <- y @>=# 1.1
  objective <- minimize $ x @+@ y
  status <- optimizeIP

  -- Check that we reached optimality
  liftIO $ status @?= Optimal

  vx <- getVariableValue x
  let expectedX = 2
      xmsg = printf "Expected x to be 2, but is %.3f" vx
  liftIO $ assertBool xmsg (abs (vx - expectedX) <= 1e-3)

  vy <- getVariableValue y
  let expectedY = 1.1
      ymsg = printf "Expected y to be 1.1, but is %.3f" vy
  liftIO $ assertBool ymsg (abs (vy - expectedY) <= 1e-3)

  vobj <- getObjectiveValue objective
  let expectedObj = expectedX + expectedY
      objMsg = printf "Expected optimal solution to be %f, but is %f" expectedObj vobj
  liftIO $ assertBool objMsg (abs (vobj - expectedObj) <= 1e-3)
