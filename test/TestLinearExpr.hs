{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language FlexibleInstances #-}
module TestLinearExpr where

import Data.Ratio

import Test.Tasty
import Test.Tasty.QuickCheck

import Math.Programming.Expr

test_tree :: TestTree
test_tree = testGroup "LinearExpression tests"
  [ testProperty "Commutativity" commutativityProp ]

type ExactExpr = LinearExpr (Ratio Integer) (Ratio Integer)

instance Arbitrary ExactExpr where
  arbitrary = LinearExpr <$> arbitrary <*> arbitrary

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

commutativityProp :: ShuffledAndUnshuffled -> Bool
commutativityProp (ShuffledAndUnshuffled (shuffled, unshuffled))
  = eval shuffled == eval unshuffled
