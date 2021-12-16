{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.Programming.Tests.LP where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Math.Programming
import Test.Hspec
import Text.Printf

makeLPTests ::
  (MonadIO m, LPMonad v c o m) =>
  -- | The runner for the API being tested.
  (m () -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeLPTests runner =
  describe "LP problems" $ do
    it "solves the diet problem" (runner dietProblemTest)

data Food = Corn | Milk | Bread
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

data Nutrient = Calories | VitaminA
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

dietProblemTest :: (MonadIO m, LPMonad v c o m) => m ()
dietProblemTest =
  let cost :: Food -> Double
      cost Corn = 0.18
      cost Milk = 0.23
      cost Bread = 0.05

      nutrition :: Nutrient -> Food -> Double
      nutrition Calories Corn = 72
      nutrition VitaminA Corn = 107
      nutrition Calories Milk = 121
      nutrition VitaminA Milk = 500
      nutrition Calories Bread = 65
      nutrition VitaminA Bread = 0

      foods :: [Food]
      foods = [Corn, Milk, Bread]

      nutrients :: [Nutrient]
      nutrients = [Calories, VitaminA]

      maxServings :: Double
      maxServings = 10

      nutrientBounds :: Nutrient -> (Double, Double)
      nutrientBounds Calories = (2000, 2250)
      nutrientBounds VitaminA = (5000, 50000)

      expected :: Food -> Double
      expected Corn = 1.94
      expected Milk = 10
      expected Bread = 10

      expectedCost :: Double
      expectedCost = 3.15

      amountInterval :: Bounds
      amountInterval = Interval 0 maxServings

      amountName :: Food -> T.Text
      amountName food = T.pack $ printf "amount[%s]" (show food)

      nutrientMaxName :: Nutrient -> T.Text
      nutrientMaxName nutrient = T.pack $ printf "%s_max" (show nutrient)

      nutrientMinName :: Nutrient -> T.Text
      nutrientMinName nutrient = T.pack $ printf "%s_min" (show nutrient)
   in do
        -- Create the decision variables
        amounts <- forM foods $ \food -> do
          v <- free `within` amountInterval
          setVariableName v (amountName food)
          return (food, v)

        -- Create the nutrient constraints
        forM_ nutrients $ \nutrient -> do
          let lhs = esum [nutrition nutrient food .* v | (food, v) <- amounts]
              (lower, upper) = nutrientBounds nutrient
          cl <- lhs .<=# upper
          setConstraintName cl (nutrientMaxName nutrient)
          cu <- lhs .>=# lower
          setConstraintName cu (nutrientMinName nutrient)
          pure ()

        -- Set the objective
        let objectiveExpr = esum [cost food .* v | (food, v) <- amounts]
        objective <- addObjective objectiveExpr
        setSense objective Minimization

        -- Solve the problem
        status <- optimizeLP

        -- Check that we reached optimality
        liftIO $ status `shouldBe` Optimal

        -- Check the variable values
        forM_ amounts $ \(food, v) -> do
          x <- getVariableValue v
          liftIO $ abs (x - expected food) `shouldSatisfy` (<= 1e-1)

        -- Check the objective value
        objectiveValue <- evalExpr objectiveExpr
        liftIO $ abs (objectiveValue - expectedCost) `shouldSatisfy` (<= 1e-1)
