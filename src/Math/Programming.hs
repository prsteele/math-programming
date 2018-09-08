{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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

class (Monad m, Num (Numeric m)) => LPMonad m where

  -- | The type of variables in the model. The LPMonad treats these as
  -- opaque values, but instances may expose more details.
  data Variable m :: *

  -- | The type of constraints in the model. The LPMonad treats these
  -- as opaque values, but instances may expose more details.
  data Constraint m :: *

  -- | The numeric type used in the model.
  type Numeric m

  -- | Create a new decision variable in the model.
  addVariable :: m (Variable m)

  -- | Name an existing decision variable in the model.
  nameVariable :: Variable m -> String -> m ()

  -- | Delete a decision variable from the model. The variable should
  -- no longer be used after being deleted.
  deleteVariable :: Variable m -> m ()

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality (Variable m) (Numeric m) -> m (Constraint m)

  -- | Name an existing constraint in the model.
  nameConstraint :: Constraint m -> String -> m ()

  -- | Delete a constraint from the model. The constraint should no
  -- longer be used after being deleted.
  deleteConstraint :: Constraint m -> m ()

  -- | Set the objective function of the model.
  setObjective :: LinearExpr (Variable m) (Numeric m) -> m ()

  -- | Set the optimization direction of the model.
  setSense :: Sense -> m ()

  -- | Optimize the model.
  optimize :: m SolutionStatus

  -- | Set the upper- or lower-bounds on a variable.
  setVariableBounds :: Variable m -> Bounds (Numeric m) -> m ()

  -- | Set the domain of a variable, i.e. whether it is continuous or
  -- integral.
  setVariableDomain :: Variable m -> Domain -> m ()

  -- | Get the value of a variable in the current solution.
  evaluateVariable :: Variable m -> m (Numeric m)

  -- | Get the value of a linear expression in the current solution.
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
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

-- | Constrain a variable to take on certain values.
within :: (LPMonad m) => m (Variable m) -> Bounds (Numeric m) -> m (Variable m)
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable

-- | Set the type of a variable.
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

instance (LPMonad m) => Named m (Constraint m) where
  named mkConstraint name = do
    constraint <- mkConstraint
    nameConstraint constraint name
    return constraint
