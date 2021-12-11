module Main where

import Test.Tasty

import qualified ApiTests as Api
import qualified RegressionTests as Regression

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
        [ Api.test_tree
        , Regression.test_tree
        ]
