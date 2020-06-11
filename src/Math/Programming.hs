{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Math.Programming
  ( -- * Linear programs
    LPMonad (..)
  , Sense (..)
  , Bounds (..)
  , within
  , Named (..)
    -- * Building linear expressions
  , module Math.Programming.Expression
    -- * Building constraints
  ) where

import           Data.Proxy

import           Math.Programming.Constraint
import           Math.Programming.Expression

-- | A monad for formulating and solving linear programs.
class (Num (Numeric m), Monad m) => LPMonad m where
  -- | The numeric type used in the model.
  type Numeric m :: *

  -- | The type of variables in the model. The LPMonad treats these as
  -- opaque values, but instances may expose more details.
  data Variable m :: *

  -- | The type of constraints in the model. The LPMonad treats these
  -- as opaque values, but instances may expose more details.
  data Constraint m :: *

  -- | Create a new decision variable in the model.
  --
  -- This variable will be initialized to be a non-negative continuous
  -- variable.
  addVariable :: m (Variable m)

  -- | Associate a name with a decision variable.
  setVariableName :: Variable m -> String -> m ()

  -- | Retrieve the name of a variable.
  getVariableName :: Variable m -> m String

  -- | Delete a decision variable from the model.
  --
  -- The variable cannot be used after being deleted.
  deleteVariable :: Variable m -> m ()

  -- | Set the upper- or lower-bounds on a variable.
  setVariableBounds :: Variable m -> Bounds (Numeric m) -> m ()

  -- | Get the upper and lower-bounds on a variable.
  getVariableBounds :: Variable m -> m (Bounds (Numeric m))

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality (LinearExpr (Numeric m) (Variable m)) -> m (Constraint m)

  -- | Associate a name with a constraint.
  setConstraintName :: Constraint m -> String -> m ()

  -- Retrieve the name of the constraint.
  getConstraintName :: Constraint m -> m String

  -- | Delete a constraint from the model.
  --
  -- The constraint cannot used after being deleted.
  deleteConstraint :: Constraint m -> m ()

  -- | Set the objective function of the model.
  setObjective :: LinearExpr (Numeric m) (Variable m) -> m ()

  -- | Set the optimization direction of the model.
  setSense :: Sense -> m ()

  -- | Optimize the continuous relaxation of the model.
  optimizeLP :: m SolutionStatus

  -- | Set the optimization timeout, in seconds.
  setTimeout :: Double -> m ()

  -- | Get the optimization timeout, in seconds.
  getTimeout :: m Double

  -- | Get the value of a variable in the current solution.
  getValue :: Variable m -> m (Numeric m)

  -- | Get the value of a linear expression in the current solution.
  evalExpr :: LinearExpr (Numeric m) (Variable m) -> m (Numeric m)
  evalExpr expr = traverse getValue expr >>= return . eval

  -- | Write out the formulation.
  writeFormulation :: FilePath -> m ()

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'LPMonad' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class LPMonad m => IPMonad m where
  -- | Optimize the mixed-integer program.
  optimizeIP :: m SolutionStatus

  -- | Set the domain of a variable, i.e. whether it is continuous or
  -- discrete.
  setVariableDomain :: Variable m -> Domain -> m ()

  -- | Get the domain of a variable, i.e. whether it is continuous or
  -- discrete.
  getVariableDomain :: Variable m -> m Domain

  -- | Set the relative MIP gap tolerance.
  setRelativeMIPGap :: Double -> m ()

  -- | Get the relative MIP gap tolerance.
  getRelativeMIPGap :: m Double

-- | An interval of the real numbers.
data Bounds b
  = NonNegativeReals
  -- ^ The interval @[0, Infinity]@
  | NonPositiveReals
  -- ^ The interval @[-Infinity, 0]@
  | Interval b b
  -- ^ Some interval @[a, b]@
  | Free
  -- ^ The interval @[-Infinity, Infinity]@
  deriving
    ( Read
    , Show
    )

-- | Whether a math program is minimizing or maximizing its objective.
data Sense = Minimization | Maximization
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

-- | The final status of an optimization.
data SolutionStatus
  = Optimal
  -- ^ An optimal solution has been found.
  | Feasible
  -- ^ A feasible solution has been found. The result may or may not
  -- be optimal.
  | Infeasible
  -- ^ The model has been proven to be infeasible.
  | Unbounded
  -- ^ The model has been proven to be unbounded.
  | Error
  -- ^ An error was encountered during the solve. Instance-specific
  -- methods should be used to determine what occurred.
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

-- | Constrain a variable to take on certain values.
within :: LPMonad m => m (Variable m) -> Bounds (Numeric m) -> m (Variable m)
within make bounds = do
  variable <- make
  setVariableBounds variable bounds
  return variable

-- | The class of objects that can be named by in a math program.
--
-- The 'named' method can be used to set the names of variables,
-- constraints, and objectives.
class LPMonad m => Named m a where
  named :: m a -> String -> m a
  getName :: a -> m String

instance LPMonad m => Named m (Variable m) where
  named mkVariable name = do
    variable <- mkVariable
    setVariableName variable name
    return variable

  getName = getVariableName

instance LPMonad m => Named m (Constraint m) where
  named mkConstraint name = do
    constraint <- mkConstraint
    setConstraintName constraint name
    return constraint

  getName = getConstraintName

-- | The type of values that a variable can take on.
data Domain
  = Continuous
  -- ^ The variable lies in the real numbers
  | Integer
  -- ^ The variable lies in the integers
  | Binary
  -- ^ The variable lies in the set @{0, 1}@.
  deriving
    ( Read
    , Show
    )

-- | Set the type of a variable.
asKind :: IPMonad m => m (Variable m) -> Domain -> m (Variable m)
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  return variable


