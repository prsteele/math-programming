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
  ) where

import Math.Programming.Expr
import Math.Programming.Constraint

newtype Variable = Variable Int

data VariableBounds
  = NonNegative
  | NonPositive
  | Interval Double Double
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
  setVariableBounds :: Variable -> VariableBounds -> m ()
