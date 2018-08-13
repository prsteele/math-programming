{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Programming
  ( module Math.Programming.Expr
  , module Math.Programming.Constraint
  , Variable (..)
  , Constraint (..)
  , Sense (..)
  , SolutionStatus (..)
  , LPMonad (..)
  , VariableBounds (..)
  , within
  ) where

import Math.Programming.Constraint
import Math.Programming.Expr

newtype Variable = Variable Int

data VariableBounds b
  = NonNegativeReals
  | NonPositiveReals
  | Interval b b
  | Free

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
  setVariableBounds :: Variable -> VariableBounds b -> m ()

within :: (LPMonad m b) => m Variable -> VariableBounds b -> m Variable
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable
