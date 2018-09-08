{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Math.Programming
  ( module Math.Programming.Expr
  , module Math.Programming.Inequality
  , Variable (..)
  , Inequality (..)
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

import Math.Programming.Expr
import Math.Programming.Inequality

class (Monad m, Num b) => LPMonad m b | m -> b where
  -- | The type of variables in the model. The LPMonad treats these as
  -- opaque values, but instances may expose more details.
  data Variable m :: *

  -- | The type of constraints in the model. The LPMonad treats these
  -- as opaque values, but instances may expose more details.
  data Constraint m :: *

  -- | Create a new decision variable in the model.
  addVariable :: m (Variable m)

  -- | Name an existing decision variable in the model.
  nameVariable :: Variable m -> String -> m ()

  -- | Delete a decision variable from the model. The variable should
  -- no longer be used after being deleted.
  deleteVariable :: Variable m -> m ()

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality (Variable m) b -> m (Constraint m)

  -- | Name an existing constraint in the model.
  nameConstraint :: Constraint m -> String -> m ()

  -- | Delete a constraint from the model. The constraint should no
  -- longer be used after being deleted.
  deleteConstraint :: Constraint m -> m ()

  -- | Set the objective function of the model.
  setObjective :: LinearExpr (Variable m) b -> m ()

  -- | Set the optimization direction of the model.
  setSense :: Sense -> m ()

  -- | Optimize the model.
  optimize :: m SolutionStatus

  -- | Set the upper- or lower-bounds on a variable.
  setVariableBounds :: Variable m -> Bounds b -> m ()

  -- | Set the domain of a variable, i.e. whether it is continuous or
  -- integral.
  setVariableDomain :: Variable m -> Domain -> m ()

  -- | Get the value of a variable in the current solution.
  evaluateVariable :: Variable m -> m b

  -- | Get the value of a linear expression in the current solution.
  evaluateExpression :: LinearExpr (Variable m) b -> m b

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
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

-- | Constrain a variable to take on certain values.
within :: (LPMonad m b) => m (Variable m) -> Bounds b -> m (Variable m)
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable

-- | Set the type of a variable.
asKind :: (LPMonad m b) => m (Variable m) -> Domain -> m (Variable m)
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  return variable

class (LPMonad m b) => Eval m a b where
  evaluate :: a -> m b

instance (LPMonad m b) => Eval m (Variable m) b where
  evaluate = evaluateVariable

instance (LPMonad m b) => Eval m (LinearExpr (Variable m) b) b where
  evaluate = evaluateExpression

class (LPMonad m b) => Named m a b where
  named :: m a -> String -> m a

instance (LPMonad m b) => Named m (Variable m) b where
  named mkVariable name = do
    variable <- mkVariable
    nameVariable variable name
    return variable

instance (LPMonad m b) => Named m (Constraint m) b where
  named mkConstraint name = do
    constraint <- mkConstraint
    nameConstraint constraint name
    return constraint
