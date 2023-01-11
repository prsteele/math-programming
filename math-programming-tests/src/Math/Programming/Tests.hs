{-# LANGUAGE FlexibleContexts #-}

module Math.Programming.Tests where

import Control.Monad.IO.Class
import Math.Programming
import Math.Programming.Tests.Api
import Math.Programming.Tests.Fuzz
import Math.Programming.Tests.IP
import Math.Programming.Tests.LP
import Test.Hspec
import Text.Printf

makeAllTests ::
  (MonadIO m, MonadIP v c o m) =>
  -- | The name of the API being tested. This will
  -- be used to generate test group names.
  String ->
  -- | The runner for the API being tested.
  (m () -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeAllTests apiName runner =
  describe (printf "Math.Programming API tests (%s backend)" apiName) $ do
    makeApiTests runner
    makeLPTests runner
    makeIPTests runner
    makeFuzzTests runner
