{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
{-|
Module : Main

The entry point for tests. We use tasty-discover to automatically
discover test functions in this directory, so simply preface tasty
tests with test_ for tasty TestTrees, or prop_ for QuickCheck
properties.
-}
module Main where
