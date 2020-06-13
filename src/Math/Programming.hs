{-| A library for modeling and solving linear and integer programs.

This library is merely a frontend to various solver backends. At the
time this was written, the only known supported backend is
<https://github.com/prsteele/math-programming-glpk GLPK>.
-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Math.Programming
  ( -- * Linear programs
    LPMonad (..)
  , Sense (..)
  , SolutionStatus (..)
  -- * (Mixed) integer programs
  , IPMonad (..)
  , Domain (..)
  -- ** Utilities
  , Bounds (..)
  , within
  , asKind
  , evalExpr
  , named
  , nameOf
  , free
  , bounded
  , nonNeg
  , nonPos
    -- * Building linear expressions
  , module Math.Programming.Expression
    -- * Building constraints
  , module Math.Programming.Constraint
  ) where

import           Math.Programming.Constraint
import           Math.Programming.Expression

-- | A monad for formulating and solving linear programs.
--
-- We manipulate linear programs and their settings using the
-- 'Mutable' typeclass.
class (Monad m, Num (Numeric m)) => LPMonad m where
  -- | The numeric type used in the model.
  type Numeric m :: *

  -- | The type of variables in the model. 'LPMonad' treats these as
  -- opaque values, but instances may expose more details.
  data Variable m :: *

  -- | The type of constraints in the model. 'LPMonad' treats these
  -- as opaque values, but instances may expose more details.
  data Constraint m :: *

  -- | The type of objectives in the model. 'LPMonad' treats these
  -- as opaque values, but instances may expose more details.
  data Objective m :: *

  -- | Create a new decision variable in the model.
  --
  -- This variable will be initialized to be a non-negative continuous
  -- variable.
  addVariable :: m (Variable m)

  -- | Remove a decision variable from the model.
  --
  -- The variable cannot be used after being deleted.
  removeVariable :: Variable m -> m ()

  getVariableName :: Variable m -> m String

  setVariableName :: Variable m -> String -> m ()

  getVariableBounds :: Variable m -> m (Bounds (Numeric m))

  setVariableBounds :: Variable m -> Bounds (Numeric m) -> m ()

  getVariableValue :: Variable m -> m (Numeric m)

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality (LinearExpr (Numeric m) (Variable m)) -> m (Constraint m)

  -- | Remove a constraint from the model.
  --
  -- The constraint cannot used after being deleted.
  removeConstraint :: Constraint m -> m ()

  getConstraintName :: Constraint m -> m String

  setConstraintName :: Constraint m -> String -> m ()

  -- | Add a constraint to the model represented by an inequality.
  addObjective :: LinearExpr (Numeric m) (Variable m) -> m (Objective m)

  getObjectiveName :: Objective m -> m String

  setObjectiveName :: Objective m -> String -> m ()

  getObjectiveSense :: Objective m -> m Sense
  setObjectiveSense :: Objective m -> Sense -> m ()

  getTimeout :: m Double
  setTimeout :: Double -> m ()

  -- | Optimize the continuous relaxation of the model.
  optimizeLP :: m SolutionStatus

  -- | Write out the formulation.
  writeFormulation :: FilePath -> m ()

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'LPMonad' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class ( LPMonad m
      ) => IPMonad m where
  -- | Optimize the mixed-integer program.
  optimizeIP :: m SolutionStatus

  getVariableDomain :: Variable m -> m Domain
  setVariableDomain :: Variable m -> Domain -> m ()

  getRelativeMIPGap :: m Double
  setRelativeMIPGap :: Double -> m ()

free :: LPMonad m => m (Variable m)
free = addVariable `within` Free

nonNeg :: LPMonad m => m (Variable m)
nonNeg = addVariable `within` NonNegativeReals

nonPos :: LPMonad m => m (Variable m)
nonPos = addVariable `within` NonPositiveReals

bounded :: LPMonad m => Numeric m -> Numeric m -> m (Variable m)
bounded lo hi = within addVariable (Interval lo hi)

-- | An interval of the real numbers.
data Bounds b
  = NonNegativeReals
  -- ^ The non-negative reals.
  | NonPositiveReals
  -- ^ The non-positive reals.
  | Interval b b
  -- ^ Any closed interval of the reals.
  | Free
  -- ^ Any real number.
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
within makeVar bounds = do
  variable <- makeVar
  setVariableBounds variable bounds
  pure variable

class Nameable m a where
  getName :: a -> m String
  setName :: a -> String -> m ()

instance LPMonad m => Nameable m (Variable m) where
  getName = getVariableName
  setName = setVariableName

instance LPMonad m => Nameable m (Constraint m) where
  getName = getConstraintName
  setName = setConstraintName

instance LPMonad m => Nameable m (Objective m) where
  getName = getObjectiveName
  setName = setObjectiveName

nameOf :: (Monad m, Nameable m a) => a -> m String
nameOf = getName

named :: (Monad m, Nameable m a) => m a -> String -> m a
named make name = do
  x <- make
  setName x name
  pure x

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
  pure variable

-- | Get the value of a linear expression in the current solution.
evalExpr :: LPMonad m => LinearExpr (Numeric m) (Variable m) -> m (Numeric m)
evalExpr expr = traverse getVariableValue expr >>= return . eval
