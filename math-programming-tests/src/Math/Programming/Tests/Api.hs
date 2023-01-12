{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Math.Programming.Tests.Api where

import Control.Monad.IO.Class
import Math.Programming
import Test.Hspec

makeApiTests ::
  (MonadIO m, MonadLP v c o m) =>
  -- | The runner for the API being tested.
  (m () -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeApiTests runner =
  describe "API tests" $ do
    it "sets and gets variable names" (runner setGetVariableName)
    it "sets and gets constraint names" (runner setGetConstraintName)

setGetVariableName :: (MonadIO m, MonadLP v c o m) => m ()
setGetVariableName = do
  let name = "foo"
  x <- free
  setVariableName x name
  vName <- getVariableName x
  liftIO $ vName `shouldBe` name

setGetConstraintName :: (MonadIO m, MonadLP v c o m) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- free
  c <- var x .>= 0
  setConstraintName c name
  cName <- getConstraintName c
  liftIO $ cName `shouldBe` name
