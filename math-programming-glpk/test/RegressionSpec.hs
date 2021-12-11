module RegressionSpec where

import Control.Monad.IO.Class
import Math.Programming
import Math.Programming.Glpk
import Test.Hspec

spec :: Spec
spec =
  describe "Regression tests" $ do
    it "solves an LP with free variables" testFreeVariablesLP
    it "solves an IP with free variables" testFreeVariablesIP
    it "finds an infeasible LP to be infeasible" testInfeasibleLP
    it "finds an infeasible IP to be infeasible" testInfeasibleIP

assertFeasible :: SolutionStatus -> Glpk ()
assertFeasible result =
  liftIO $ case result of
    Error -> expectationFailure "Failed to solve program"
    Unbounded -> expectationFailure "Unbounded program"
    Infeasible -> expectationFailure "Infeasible program"
    _ -> pure ()

testFreeVariablesLP :: IO ()
testFreeVariablesLP = runGlpk $ do
  x <- free
  y <- free
  z <- free

  _ <- x @==# 0
  _ <- y @==# 3.1
  _ <- z @==# -3.1

  optimizeLP >>= assertFeasible

  vx <- getVariableValue x
  vy <- getVariableValue y
  vz <- getVariableValue z

  liftIO $ 0 `shouldBe` vx
  liftIO $ 3.1 `shouldBe` vy
  liftIO $ -3.1 `shouldBe` vz

testFreeVariablesIP :: IO ()
testFreeVariablesIP = runGlpk $ do
  x <- integer
  y <- integer
  z <- integer

  _ <- x @==# 0
  _ <- y @==# 3
  _ <- z @==# -3

  optimizeIP >>= assertFeasible

  vx <- getVariableValue x
  vy <- getVariableValue y
  vz <- getVariableValue z

  liftIO $ 0 `shouldBe` vx
  liftIO $ 3 `shouldBe` vy
  liftIO $ -3 `shouldBe` vz

testInfeasibleLP :: IO ()
testInfeasibleLP = runGlpk $ do
  x <- free
  _ <- x @>=# 2
  _ <- x @<=# 1

  status <- optimizeLP

  liftIO $ Infeasible `shouldBe` status

testInfeasibleIP :: IO ()
testInfeasibleIP = runGlpk $ do
  x <- integer
  _ <- x @>=# 2
  _ <- x @<=# 1

  status <- optimizeIP

  liftIO $ Infeasible `shouldBe` status
