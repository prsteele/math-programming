module Math.Programming
  ( module Math.Programming.Expr
  , Variable (..)
  , Constraint (..)
  , Sense (..)
  , SolutionStatus (..)
  , LPMonad (..)
  ) where

import Math.Programming.Expr

newtype Variable = Variable Int

data Constraint a
  = Constraint (LinearExpr a) Ordering

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

class Monad m => LPMonad m where
  makeVariable :: m Variable
  addConstraint :: Constraint Variable -> m ()
  setSense :: Sense -> m ()
  optimize :: m SolutionStatus
