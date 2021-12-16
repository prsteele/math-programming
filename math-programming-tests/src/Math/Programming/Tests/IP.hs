{-# LANGUAGE FlexibleContexts #-}

module Math.Programming.Tests.IP where

import Control.Monad.IO.Class
import Math.Programming
import Test.Hspec

makeIPTests ::
  (MonadIO m, IPMonad v c o m) =>
  -- | The runner for the API being tested.
  (m () -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeIPTests runner =
  describe "IP problems" $ do
    it "solves a simple MIP" (runner simpleMIPTest)

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
simpleMIPTest :: (MonadIO m, IPMonad v c o m) => m ()
simpleMIPTest = do
  x <- bounded 0 5 `asKind` Integer
  y <- bounded 0 5 `asKind` Continuous
  _ <- x @>=# 1.1
  _ <- y @>=# 1.1
  objective <- minimize $ var x .+ var y
  status <- optimizeIP

  -- Check that we reached optimality
  liftIO $ status `shouldBe` Optimal

  let expectedX = 2
      expectedY = 1.1
      expectedObj = expectedX + expectedY

  vx <- getVariableValue x
  liftIO $ abs (vx - expectedX) `shouldSatisfy` (<= 1e-3)

  vy <- getVariableValue y
  liftIO $ abs (vy - expectedY) `shouldSatisfy` (<= 1e-3)

  vobj <- getObjectiveValue objective
  liftIO $ abs (vobj - expectedObj) `shouldSatisfy` (<= 1e-3)
