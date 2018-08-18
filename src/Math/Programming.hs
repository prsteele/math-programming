{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
  , Eval (..)
  , Named (..)
  ) where

import Math.Programming.Constraint
import Math.Programming.Expr

class (Monad m, Num (Numeric m)) => LPMonad m where
  data Variable m :: *
  data ConstraintId m :: *
  type Numeric m

  addVariable :: m (Variable m)
  nameVariable :: Variable m -> String -> m ()
  deleteVariable :: Variable m -> m ()
  addConstraint :: Constraint (Variable m) (Numeric m) -> m (ConstraintId m)
  nameConstraint :: ConstraintId m -> String -> m ()
  deleteConstraint :: ConstraintId m -> m ()
  setObjective :: LinearExpr (Variable m) (Numeric m) -> m ()
  setSense :: Sense -> m ()
  optimize :: m SolutionStatus
  setVariableBounds :: Variable m -> Bounds (Numeric m) -> m ()
  setVariableDomain :: Variable m -> Domain -> m ()
  evaluateVariable :: Variable m -> m (Numeric m)
  evaluateExpression :: LinearExpr (Variable m) (Numeric m) -> m (Numeric m)

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



addIntegerVariable :: (LPMonad m) => m (Variable m)
addIntegerVariable = addVariable `asKind` Integer

addBinaryVariable :: (LPMonad m) => m (Variable m)
addBinaryVariable = addVariable `asKind` Binary

within :: (LPMonad m) => m (Variable m) -> Bounds (Numeric m) -> m (Variable m)
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable

asKind :: (LPMonad m) => m (Variable m) -> Domain -> m (Variable m)
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  return variable

class (LPMonad m) => Eval m a where
  evaluate :: a -> m (Numeric m)

instance (LPMonad m) => Eval m (Variable m) where
  evaluate = evaluateVariable

instance (LPMonad m, b ~ Numeric m) => Eval m (LinearExpr (Variable m) b) where
  evaluate = evaluateExpression

class (LPMonad m) => Named m a where
  named :: m a -> String -> m a

instance (LPMonad m) => Named m (Variable m) where
  named mkVariable name = do
    variable <- mkVariable
    nameVariable variable name
    return variable

instance (LPMonad m) => Named m (ConstraintId m) where
  named mkConstraintId name = do
    constraintId <- mkConstraintId
    nameConstraint constraintId name
    return constraintId
