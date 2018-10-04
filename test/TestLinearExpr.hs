{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module TestLinearExpr where

import Control.Monad.Reader
import qualified Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Math.Programming

newtype MockLP a = MockLP { _runMockLP :: ReaderT (M.Map (Variable MockLP) Double) IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (M.Map (Variable MockLP) Double)
    )

getVar :: Variable MockLP -> MockLP Double
getVar var = asks (M.findWithDefault (read "NaN") var)

runMockLP :: M.Map (Variable MockLP) Double -> MockLP a -> IO a
runMockLP vars actions = runReaderT (_runMockLP actions) vars

instance LPMonad MockLP Double where
  data Variable MockLP = Variable { fromVariable :: String }
                       deriving
                         ( Eq
                         , Ord
                         , Show
                         )
  data Constraint MockLP = Constraint

  eval = getVar

  addVariable = error "MockLP addVariable"
  nameVariable = error "MockLP nameVariable"
  deleteVariable = error "MockLP deleteVariable"
  addConstraint = error "MockLP addConstraint"
  nameConstraint = error "MockLP nameConstraint"
  deleteConstraint = error "MockLP deleteConstraint"
  setObjective = error "MockLP setObjective"
  setSense = error "MockLP setSense"
  optimizeLP = error "MockLP optimizeLP"
  setVariableBounds = error "MockLP setVariableBounds"
  setTimeout = error "MockLP setTimeout"
  writeFormulation = error "MockLP writeFormulation"

test_simple_expressions :: TestTree
test_simple_expressions = testGroup "Simple expressions"
  [ testCase "Constants" constantTest
  ]

example :: M.Map (Variable MockLP) Double
example = M.fromList
  [ (Variable "w", 0)
  , (Variable "x", 1)
  , (Variable "y", 2)
  , (Variable "z", 3)
  ]

constantTest :: IO ()
constantTest = runMockLP example $ do
  value <- evalExpr $ 1.0 *: Variable "x" .+. 2.0 *: Variable "y"
  liftIO $ value @?= 5.0
