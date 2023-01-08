{-# LANGUAGE OverloadedStrings #-}

module Math.Programming.DslSpec where

import Control.Monad.Identity
import qualified Data.Text as T
import Math.Programming
import Math.Programming.Dsl
import Test.Hspec

spec :: Spec
spec = do
  describe "formatExpr" $ do
    it "formats expressions correctly" $ do
      let ex :: Expr T.Text
          ex = 2 *. "x" .+ 3 *. "y" .+ con 1
      formatExpr' pure ex `shouldBe` Identity "2.0 * x + 3.0 * y + 1.0"

      let ex2 :: Expr T.Text
          ex2 = con 0
      formatExpr' pure ex2 `shouldBe` Identity "0.0"
