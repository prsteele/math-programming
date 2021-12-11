{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Math.Programming.Tests.Api where

import Control.Monad.IO.Class
import Data.Functor (void)
import Math.Programming
import Test.Hspec
import Test.QuickCheck

makeApiTests ::
  (Num (Numeric m), MonadIO m, LPMonad m) =>
  -- | The runner for the API being tested.
  (m () -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeApiTests runner =
  describe "API tests" $ do
    it "sets and gets variable names" (runner setGetVariableName)
    it "sets and gets constraint names" (runner setGetConstraintName)

-- | We should be able to set and retrieve variable names
setGetVariableName :: (MonadIO m, LPMonad m) => m ()
setGetVariableName = do
  let name = "foo"
  x <- free
  setVariableName x name

  vName <- getVariableName x
  liftIO $ vName `shouldBe` name

-- | We should be able to set and retrieve constraint names
setGetConstraintName :: (Num (Numeric m), MonadIO m, LPMonad m) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- free
  c <- (x @>=# 0) `named` name
  cName <- getConstraintName c
  liftIO $ cName `shouldBe` name

data Action
  = AddVariable
  | AddConstraint
  | AddThenDeleteVariable
  | AddThenDeleteConstraint
  deriving
    ( Enum,
      Show
    )

instance Arbitrary Action where
  arbitrary = elements actions
    where
      actions = [AddVariable .. AddThenDeleteConstraint]

newtype LPActions m = LPActions (m ())

instance LPMonad m => Arbitrary (LPActions m) where
  arbitrary = LPActions <$> sized lpActions

lpActions :: LPMonad m => Int -> Gen (m ())
lpActions remaining
  | remaining <= 0 = return (return ())
  | otherwise = do
    action <- arbitrary
    case action of
      AddVariable ->
        return (void addVariable)
      AddThenDeleteVariable ->
        bindOver addVariable removeVariable <$> lpActions (remaining - 1)
      _ -> return (return ())

-- | Execute the monadic bind (>>=), with some other actions taken in
-- between.
bindOver ::
  (Monad m) =>
  -- | The action providing the passed value
  m a ->
  -- | The function to bind to
  (a -> m b) ->
  -- | The intermediate actions
  m () ->
  -- | The resulting value
  m b
bindOver action fn intermediate = action >>= (\x -> intermediate >> fn x)

arbitraryLPActionsProp :: LPActions m -> m ()
arbitraryLPActionsProp (LPActions actions) = actions
