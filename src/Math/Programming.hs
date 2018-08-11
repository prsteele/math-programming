module Math.Programming
  ( module Math.Programming.Expr
  , module Math.Programming.Constraint
  , Variable (..)
  , Constraint (..)
  , Sense (..)
  , SolutionStatus (..)
  , LPMonad (..)
  ) where

import Math.Programming.Expr
import Math.Programming.Constraint

newtype Variable = Variable Int

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
