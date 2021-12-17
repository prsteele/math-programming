{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

-- | We should be able to set and retrieve variable names
setGetVariableName :: (MonadIO m, MonadLP v c o m) => m ()
setGetVariableName = do
  let name = "foo"
  x <- free `named` name
  vName <- getName x
  liftIO $ vName `shouldBe` name

-- | We should be able to set and retrieve constraint names
setGetConstraintName :: (MonadIO m, MonadLP v c o m) => m ()
setGetConstraintName = do
  let name = "foo"
  x <- free
  c <- (var x .>=# 0) `named` name
  cName <- getName c
  liftIO $ cName `shouldBe` name
