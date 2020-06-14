{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Math.Programming.Types where

import           Data.Bifunctor
import           Data.Traversable (fmapDefault, foldMapDefault)

-- | A convient shorthand for the type of linear expressions used in a
-- given model.
type Expr m = LinearExpression (Numeric m) (Variable m)

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

  -- | Get the name of the variable.
  getVariableName :: Variable m -> m String

  -- | Set the name of the variable.
  setVariableName :: Variable m -> String -> m ()

  -- | Get the allowed values of a variable.
  getVariableBounds :: Variable m -> m (Bounds (Numeric m))

  -- | Constrain a variable to take on certain values.
  setVariableBounds :: Variable m -> Bounds (Numeric m) -> m ()

  -- | Get the value of a variable in the current solution.
  getVariableValue :: Variable m -> m (Numeric m)

  -- | Add a constraint to the model represented by an inequality.
  addConstraint :: Inequality (LinearExpression (Numeric m) (Variable m)) -> m (Constraint m)

  -- | Remove a constraint from the model.
  --
  -- The constraint cannot used after being deleted.
  removeConstraint :: Constraint m -> m ()

  -- | Get the name of the constraint.
  getConstraintName :: Constraint m -> m String

  -- | Set the name of the constraint.
  setConstraintName :: Constraint m -> String -> m ()

  -- | Get the value of the dual variable associated with the
  -- constraint in the current solution.
  --
  -- This value has no meaning if the current solution is not an LP
  -- solution.
  getDualValue :: Constraint m -> m (Numeric m)

  -- | Add a constraint to the model represented by an inequality.
  addObjective :: LinearExpression (Numeric m) (Variable m) -> m (Objective m)

  -- | Get the name of the objective.
  getObjectiveName :: Objective m -> m String

  -- | Set the name of the objective.
  setObjectiveName :: Objective m -> String -> m ()

  -- | Whether the objective is to be minimized or maximized.
  getObjectiveSense :: Objective m -> m Sense

  -- | Set whether the objective is to be minimized or maximized.
  setObjectiveSense :: Objective m -> Sense -> m ()

  -- | Get the value of the objective in the current solution.
  getObjectiveValue :: Objective m -> m (Numeric m)

  -- | Get the number of seconds the solver is allowed to run before
  -- halting.
  getTimeout :: m Double

  -- | Set the number of seconds the solver is allowed to run before
  -- halting.
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

  -- | Get the domain of a variable.
  getVariableDomain :: Variable m -> m Domain

  -- | Set the domain of a variable.
  setVariableDomain :: Variable m -> Domain -> m ()

  -- | Get the allowed relative gap between LP and IP solutions.
  getRelativeMIPGap :: m Double

  -- | Set the allowed relative gap between LP and IP solutions.
  setRelativeMIPGap :: Double -> m ()

-- | Whether a math program is minimizing or maximizing its objective.
data Sense = Minimization | Maximization
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

-- | The outcome of an optimization.
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

-- | The type of values that a variable can take on.
--
-- Note that the @Integer@ constructor does not interfere with the
-- @Integer@ type, as the @Integer@ type does not define a constuctor
-- of the same name. The ambiguity is unfortunate, but other natural
-- nomenclature such as @Integral@ are similarly conflicted.
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

-- | A linear expression containing symbolic variables of type @b@ and
-- numeric coefficients of type @a@.
--
-- Using 'String's to denote variables and 'Double's as our numeric
-- type, we could express /3 x + 2 y + 1/ as
--
-- @
--   LinearExpression [(3, "x"), (2, "y")] 1
-- @
data LinearExpression a b
  = LinearExpression [(a, b)] a
  deriving
    ( Read
    , Show
    )

-- | Implements addition of 'LinearExpression a b' terms
instance Num a => Semigroup (LinearExpression a b) where
  (LinearExpression termsLhs constantLhs) <> (LinearExpression termsRhs constantRhs)
    = LinearExpression (termsLhs <> termsRhs) (constantLhs + constantRhs)

-- | Using '0' as the identity element
instance Num a => Monoid (LinearExpression a b) where
  mempty = LinearExpression [] 0

instance Functor (LinearExpression a) where
  fmap = fmapDefault

instance Bifunctor LinearExpression where
  first f (LinearExpression terms constant)
    = LinearExpression (fmap (first f) terms) (f constant)
  second f (LinearExpression terms constant)
    = LinearExpression (fmap (fmap f) terms) constant

instance Foldable (LinearExpression a) where
  foldMap = foldMapDefault

-- | Useful for substituting values in a monadic/applicative context
instance Traversable (LinearExpression a) where
  traverse f (LinearExpression terms constant)
    = LinearExpression <$> traverse (traverse f) terms <*> pure constant

-- | Non-strict inequalities.
data Inequality a
  = Inequality Ordering a a
  deriving
    ( Read
    , Show
    )

instance Functor Inequality where
  fmap = fmapDefault

instance Foldable Inequality where
  foldMap = foldMapDefault

instance Traversable Inequality where
  traverse f (Inequality sense lhs rhs)
    = Inequality sense <$> f lhs <*> f rhs
