{-# LANGUAGE FlexibleContexts #-}
module Math.Programming.Tests where

import           Control.Monad.IO.Class
import           Test.Tasty
import           Text.Printf

import           Math.Programming
import           Math.Programming.Tests.Api
import           Math.Programming.Tests.IP
import           Math.Programming.Tests.LP

makeAllTests
  :: (PrintfArg (Numeric m), RealFrac  (Numeric m), MonadIO m, IPMonad m)
  => String           -- ^ The name of the API being tested. This will
                      -- be used to generate test group names.
  -> (m () -> IO ())  -- ^ The runner for the API being tested.
  -> TestTree         -- ^ The resulting test suite.
makeAllTests apiName runner
  = testGroup (printf "Math.Programming tests (%s)" apiName)
    [ makeApiTests runner
    , makeLPTests runner
    , makeIPTests runner
    ]
