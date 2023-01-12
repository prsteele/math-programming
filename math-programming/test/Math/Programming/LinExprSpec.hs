{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Math.Programming.LinExprSpec where

import Control.Monad
import Data.Ratio
import Math.Programming
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "algebraic properties" $ do
    prop "satisfies addititive commutivity" commutativityProp
    prop "satisfies addititive associativity" additiveAssociativityProp
    prop "satisfies coefficient commutativity" coefficientCommutativityProp
  describe "simplification" $ do
    simplifyRegressions
    prop "simplification preserves value" simplifyProp

type ExactExpr = LinExpr (Ratio Integer) (Ratio Integer)

instance Arbitrary ExactExpr where
  arbitrary = LinExpr <$> arbitrary <*> arbitrary

-- | A pair of linear expressions, differing only by the ordering of
-- the summands.
newtype ShuffledAndUnshuffled
  = ShuffledAndUnshuffled (ExactExpr, ExactExpr)
  deriving
    ( Show
    )

instance Arbitrary ShuffledAndUnshuffled where
  arbitrary = do
    unshuffled@(LinExpr terms constant) <- arbitrary
    shuffledTerms <- shuffle terms
    let shuffled = LinExpr shuffledTerms constant
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
    unshuffled@(LinExpr terms constant) <- arbitrary
    terms' <- forM terms $ \(x, y) -> do
      flipped <- arbitrary
      return $
        if flipped
          then (y, x)
          else (x, y)
    let shuffled = LinExpr terms' constant
    return $ ShuffledCoefficients (shuffled, unshuffled)

coefficientCommutativityProp :: ShuffledCoefficients -> Bool
coefficientCommutativityProp (ShuffledCoefficients (shuffled, unshuffled)) =
  eval shuffled == eval unshuffled

additiveAssociativityProp :: ExactExpr -> ExactExpr -> ExactExpr -> Bool
additiveAssociativityProp x y z =
  eval ((x .+. y) .+. z) == eval (x .+. (y .+. z))

simplifyProp :: ExactExpr -> Bool
simplifyProp x = eval x == eval (simplify x)

simplifyRegressions :: Spec
simplifyRegressions = do
  it "simplifies x + x - x" $
    let terms :: [(Int, Int)]
        coef :: Int
        (LinExpr terms coef) = simplify $ LinExpr [(1, 0), (1, 0), (-1, 0)] 0
     in do
          coef `shouldBe` 0
          terms `shouldBe` [(1, 0)]
