{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Math.Programming.Tests.LP where

import           Control.Monad
import           Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Printf

import           Math.Programming

makeLPTests
  :: (PrintfArg (Numeric m), RealFrac (Numeric m), MonadIO m, LPMonad m)
  => (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeLPTests runner = testGroup "LP problems"
  [ testCase "Diet problem" (runner dietProblemTest)
  ]

data Food = Corn | Milk | Bread
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

data Nutrient = Calories | VitaminA
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

dietProblemTest :: forall m. (PrintfArg (Numeric m), RealFrac (Numeric m), MonadIO m, LPMonad m) => m ()
dietProblemTest =
  let
    cost :: Food -> Numeric m
    cost Corn  = 0.18
    cost Milk  = 0.23
    cost Bread = 0.05

    nutrition :: Nutrient -> Food -> Numeric m
    nutrition Calories Corn  = 72
    nutrition VitaminA Corn  = 107
    nutrition Calories Milk  = 121
    nutrition VitaminA Milk  = 500
    nutrition Calories Bread = 65
    nutrition VitaminA Bread = 0

    foods :: [Food]
    foods = [Corn, Milk, Bread]

    nutrients :: [Nutrient]
    nutrients = [Calories, VitaminA]

    maxServings :: Numeric m
    maxServings = 10

    nutrientBounds :: Nutrient -> (Numeric m, Numeric m)
    nutrientBounds Calories = (2000, 2250)
    nutrientBounds VitaminA = (5000, 50000)

    expected :: Food -> Numeric m
    expected Corn  = 1.94
    expected Milk  = 10
    expected Bread = 10

    expectedCost :: Numeric m
    expectedCost = 3.15

    amountInterval :: Bounds (Numeric m)
    amountInterval = Interval 0 maxServings

    amountName :: Food -> String
    amountName food = printf "amount[%s]" (show food)

    nutrientMaxName :: Nutrient -> String
    nutrientMaxName nutrient = printf "%s_max" (show nutrient)

    nutrientMinName :: Nutrient -> String
    nutrientMinName nutrient = printf "%s_min" (show nutrient)

  in do
    -- Create the decision variables
    amounts <- forM foods $ \food -> do
      v <- addVariable `within` amountInterval `named` (amountName food)
      return (food, v)

    -- Create the nutrient constraints
    forM_ nutrients $ \nutrient -> do
      let lhs = exprSum [nutrition nutrient food #*@ v | (food, v) <- amounts]
          (lower, upper) = nutrientBounds nutrient
      _ <- (lhs .<=# upper) `named` (nutrientMaxName nutrient)
      _ <- (lhs .>=# lower) `named` (nutrientMinName nutrient)
      pure ()

    -- Set the objective
    let objectiveExpr = exprSum [cost food #*@ v | (food, v) <- amounts]
    objective <- addObjective objectiveExpr
    setObjectiveSense objective Minimization

    -- Solve the problem
    status <- optimizeLP

    -- Check that we reached optimality
    liftIO $ status @?= Optimal

    -- Check the variable values
    forM_ amounts $ \(food, v) -> do
      x <- getVariableValue v

      let correct = expected food
          msg = printf
                "Amount of %s should be about %.2f, but is %.3f"
                (show food)
                correct
                x
      liftIO $ assertBool msg (abs (x - correct) <= 1e-1)

    -- Check the objective value
    objectiveValue <- evalExpr objectiveExpr
    let msg = printf
              "Objective should be about %.2f, but is %.3f"
              expectedCost
              objectiveValue
    liftIO $ assertBool msg (abs (objectiveValue - expectedCost) < 1e-1)
