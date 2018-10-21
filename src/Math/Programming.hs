{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Math.Programming
  (
    -- * Linear programs
    LPMonad (..)
  , Sense (..)
  , Bounds (..)
  , within
  , Named (..)
    -- ** Building linear expressions
  , module Math.Programming.Expr
    -- ** Building constraints
  , module Math.Programming.Inequality
    -- ** Evaluating results
  , SolutionStatus (..)
    -- * Integer programs
  , IPMonad (..)
  , Domain (..)
  , asKind
  ) where

import Math.Programming.Expr
import Math.Programming.Inequality

-- | A monad for formulating and solving linear programs.
--
-- The parameter 'b' is the underlying numeric type being used; this
-- will likely be the 'Double' type.
class (Monad m, Num b) => LPMonad m b | m -> b where
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
  setVariableBounds :: Variable m -> Bounds b -> m ()

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality b (Variable m) -> m (Constraint m)

  -- | Associate a name with a constraint.
  setConstraintName :: Constraint m -> String -> m ()

  -- Retrieve the name of the constraint.
  getConstraintName :: Constraint m -> m String

  -- | Delete a constraint from the model.
  --
  -- The constraint cannot used after being deleted.
  deleteConstraint :: Constraint m -> m ()

  -- | Set the objective function of the model.
  setObjective :: LinearExpr b (Variable m) -> m ()

  -- | Set the optimization direction of the model.
  setSense :: Sense -> m ()

  -- | Optimize the continuous relaxation of the model.
  optimizeLP :: m SolutionStatus

  -- | Set the optimization timeout, in seconds.
  setTimeout :: Double -> m ()

  -- | Get the value of a variable in the current solution.
  getValue :: Variable m -> m b

  -- | Get the value of a linear expression in the current solution.
  evalExpr :: LinearExpr b (Variable m) -> m b
  evalExpr expr = traverse getValue expr >>= return . eval

  -- | Write out the formulation.
  writeFormulation :: FilePath -> m ()

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'LPMonad' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class LPMonad m b => IPMonad m b where
  -- | Optimize the mixed-integer program.
  optimizeIP :: m SolutionStatus

  -- | Set the domain of a variable, i.e. whether it is continuous or
  -- discrete.
  setVariableDomain :: Variable m -> Domain -> m ()

  -- | Set the relative MIP gap tolerance.
  setRelativeMIPGap :: Double -> m ()

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
  -- methods should be used to determine what occured.
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
asKind :: (IPMonad m b) => m (Variable m) -> Domain -> m (Variable m)
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  return variable

-- | The class of objects that can be named by in a math program.
--
-- The 'named' method can be used to set the names of variables,
-- constraints, and objectives.
class (LPMonad m b) => Named m a b where
  named :: m a -> String -> m a
  getName :: a -> m String

instance (LPMonad m b) => Named m (Variable m) b where
  named mkVariable name = do
    variable <- mkVariable
    setVariableName variable name
    return variable

  getName = getVariableName

instance (LPMonad m b) => Named m (Constraint m) b where
  named mkConstraint name = do
    constraint <- mkConstraint
    setConstraintName constraint name
    return constraint

  getName = getConstraintName
