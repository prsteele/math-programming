{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleInstances #-}
module TestLinearExpr where

import Control.Monad
import Data.Ratio

import Test.Tasty
import Test.Tasty.QuickCheck

import Math.Programming.Expr

test_tree :: TestTree
test_tree = testGroup "LinearExpression tests"
  [ testProperty "Additive commutativity" commutativityProp
  , testProperty "Additive distributivity" additiveDistributivityProp
  , testProperty "Coefficient commutativity" coefficientCommutativityProp
  , testProperty "Scalar multiplicative distributivity" multiplicativeDistributivityProp
  ]

type ExactExpr = LinearExpr (Ratio Integer) (Ratio Integer)

instance Arbitrary ExactExpr where
  arbitrary = LinearExpr <$> arbitrary <*> arbitrary

-- | A pair of linear expressions, differing only by the ordering of
-- the summands.
newtype ShuffledAndUnshuffled
  = ShuffledAndUnshuffled (ExactExpr, ExactExpr)
  deriving
    ( Show
    )

instance Arbitrary ShuffledAndUnshuffled where
  arbitrary = do
    unshuffled@(LinearExpr terms constant) <- arbitrary
    shuffledTerms <- shuffle terms
    let shuffled = LinearExpr shuffledTerms constant
    return $ ShuffledAndUnshuffled (unshuffled, shuffled)

-- | Addition should be commutative.
commutativityProp :: ShuffledAndUnshuffled -> Bool
commutativityProp (ShuffledAndUnshuffled (shuffled, unshuffled))
  = eval shuffled == eval unshuffled

-- | A pair of linear expressions, differing only by the ordering of
-- the coefficients of the summands.
newtype ShuffledCoefficients
  = ShuffledCoefficients (ExactExpr, ExactExpr)
  deriving
    ( Show
    )

instance Arbitrary ShuffledCoefficients where
  arbitrary = do
    unshuffled@(LinearExpr terms constant) <- arbitrary
    terms' <- forM terms $ \(x, y) -> do
      flipped <- arbitrary
      return $ if flipped
               then (y, x)
               else (x, y)
    let shuffled = LinearExpr terms' constant
    return $ ShuffledCoefficients (shuffled, unshuffled)

coefficientCommutativityProp :: ShuffledCoefficients -> Bool
coefficientCommutativityProp (ShuffledCoefficients (shuffled, unshuffled))
  = eval shuffled == eval unshuffled

additiveDistributivityProp :: ExactExpr -> ExactExpr -> ExactExpr -> Bool
additiveDistributivityProp x y z
  = eval ((x .+. y) .+. z) == eval (x .+. (y .+. z))

multiplicativeDistributivityProp :: Ratio Integer -> ExactExpr -> Bool
multiplicativeDistributivityProp a x
  = eval (a *. x) == eval (x .* a)
