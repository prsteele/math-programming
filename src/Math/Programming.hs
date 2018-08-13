{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Programming
  ( module Math.Programming.Expr
  , module Math.Programming.Constraint
  , Variable (..)
  , Constraint (..)
  , Sense (..)
  , SolutionStatus (..)
  , LPMonad (..)
  , Bounds (..)
  , Domain (..)
  , within
  , asKind
  ) where

import Math.Programming.Constraint
import Math.Programming.Expr

newtype Variable = Variable Int

data Bounds b
  = NonNegativeReals
  | NonPositiveReals
  | Interval b b
  | Free
  deriving
    ( Read
    , Show
    )

data Domain
  = Continuous
  | Integer
  | Binary
  deriving
    ( Read
    , Show
    )

data Sense = Minimization | Maximization
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

data SolutionStatus
  = Optimal
  | Feasible
  | Infeasible
  | Unbounded
  | Error

class (Num b, Monad m) => LPMonad m b | m -> b where
  makeVariable :: m Variable
  addConstraint :: Constraint Variable b -> m ()
  setObjective :: LinearExpr Variable b -> m ()
  setSense :: Sense -> m ()
  optimize :: m SolutionStatus
  setVariableBounds :: Variable -> Bounds b -> m ()
  setVariableDomain :: Variable -> Domain -> m ()

makeIntegerVariable :: (LPMonad m b) => m Variable
makeIntegerVariable = makeVariable `asKind` Integer

makeBinaryVariable :: (LPMonad m b) => m Variable
makeBinaryVariable = makeVariable `asKind` Binary

within :: (LPMonad m b) => m Variable -> Bounds b -> m Variable
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable

asKind :: (LPMonad m b) => m Variable -> Domain -> m Variable
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  return variable
