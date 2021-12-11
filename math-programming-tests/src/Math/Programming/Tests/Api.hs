{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Programming.Tests.Api where

import           Control.Monad.IO.Class
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Math.Programming

makeApiTests
  :: (Num (Numeric m), MonadIO m, LPMonad m)
  => (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeApiTests runner = testGroup "API tests"
  [ testCase "Set/get variable names" (runner setGetVariableName)
  , testCase "Set/get constraint names" (runner setGetConstraintName)
  ]

-- | We should be able to set and retrieve variable names
setGetVariableName :: (MonadIO m, LPMonad m) => m ()
setGetVariableName = do
  let name = "foo"
  x <- free
  setVariableName x name

  vName <- getVariableName x
  liftIO $ vName @?= name

-- | We should be able to set and retrieve constraint names
setGetConstraintName :: (Num (Numeric m), MonadIO m, LPMonad m) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- free
  c <- (x @>=# 0) `named` name
  cName <- getConstraintName c
  liftIO $ cName @?= name

data Action
  = AddVariable
  | AddConstraint
  | AddThenDeleteVariable
  | AddThenDeleteConstraint
  deriving
    ( Enum
    , Show
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
  | otherwise      = do
      action <- arbitrary
      case action of
        AddVariable
          -> return (addVariable >> return ())
        AddThenDeleteVariable
          -> bindOver addVariable removeVariable <$> lpActions (remaining - 1)
        _ -> return (return ())

-- | Execute the monadic bind (>>=), with some other actions taken in
-- between.
bindOver
  :: (Monad m)
  => m a         -- ^ The action providing the passed value
  -> (a -> m b)  -- ^ The function to bind to
  -> m ()        -- ^ The intermediate actions
  -> m b         -- ^ The resulting value
bindOver action fn intermediate = action >>= (\x -> intermediate >> fn x)

arbitraryLPActionsProp :: LPActions m -> m ()
arbitraryLPActionsProp (LPActions actions) = actions
