{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Programming.LinearExpressionSpec where

import Control.Monad
import Data.Ratio
import Math.Programming
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "LinearExpression tests" $ do
  prop "satisfies addititive commutivity" commutativityProp
  prop "satisfies addititive associativity" additiveAssociativityProp
  prop "satisfies coefficient commutativity" coefficientCommutativityProp
  prop "simplifies expressions properly" simplifyProp

type ExactExpr = LinearExpression (Ratio Integer) (Ratio Integer)

instance Arbitrary ExactExpr where
  arbitrary = LinearExpression <$> arbitrary <*> arbitrary

-- | A pair of linear expressions, differing only by the ordering of
-- the summands.
newtype ShuffledAndUnshuffled
  = ShuffledAndUnshuffled (ExactExpr, ExactExpr)
  deriving
    ( Show
    )

instance Arbitrary ShuffledAndUnshuffled where
  arbitrary = do
    unshuffled@(LinearExpression terms constant) <- arbitrary
    shuffledTerms <- shuffle terms
    let shuffled = LinearExpression shuffledTerms constant
    return $ ShuffledAndUnshuffled (unshuffled, shuffled)

-- | Addition should be commutative.
commutativityProp :: ShuffledAndUnshuffled -> Bool
commutativityProp (ShuffledAndUnshuffled (shuffled, unshuffled)) =
  eval shuffled == eval unshuffled

-- | A pair of linear expressions, differing only by the ordering of
-- the coefficients of the summands.
newtype ShuffledCoefficients
  = ShuffledCoefficients (ExactExpr, ExactExpr)
  deriving
    ( Show
    )

instance Arbitrary ShuffledCoefficients where
  arbitrary = do
    unshuffled@(LinearExpression terms constant) <- arbitrary
    terms' <- forM terms $ \(x, y) -> do
      flipped <- arbitrary
      return $
        if flipped
          then (y, x)
          else (x, y)
    let shuffled = LinearExpression terms' constant
    return $ ShuffledCoefficients (shuffled, unshuffled)

coefficientCommutativityProp :: ShuffledCoefficients -> Bool
coefficientCommutativityProp (ShuffledCoefficients (shuffled, unshuffled)) =
  eval shuffled == eval unshuffled

additiveAssociativityProp :: ExactExpr -> ExactExpr -> ExactExpr -> Bool
additiveAssociativityProp x y z =
  eval ((x .+. y) .+. z) == eval (x .+. (y .+. z))

simplifyProp :: ExactExpr -> Bool
simplifyProp x = eval x == eval (simplify x)
