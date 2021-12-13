{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Programming.Types where

import Data.Bifunctor
import qualified Data.Text as T
import Data.Traversable (fmapDefault, foldMapDefault)

-- | A convient shorthand for the type of linear expressions used in a
-- given model.
type Expr = LinearExpression Double

-- | A linear program.
--
-- This is a monadic context for formulating and solving linear
-- programs.
class Monad m => LPMonad v c o m | m -> v c o where
  addVariable :: m v
  deleteVariable :: v -> m ()
  getBounds :: v -> m Bounds
  setBounds :: v -> Bounds -> m ()
  getVariableName :: v -> m T.Text
  setVariableName :: v -> T.Text -> m ()
  getVariableValue :: v -> m Double

  addConstraint :: Inequality (Expr v) -> m c
  deleteConstraint :: c -> m ()
  getConstraintName :: c -> m T.Text
  setConstraintName :: c -> T.Text -> m ()
  getConstraintValue :: c -> m Double

  addObjective :: Expr v -> m o
  deleteObjective :: o -> m ()
  getSense :: o -> m Sense
  setSense :: o -> Sense -> m ()
  getObjectiveName :: o -> m T.Text
  setObjectiveName :: o -> T.Text -> m ()
  getObjectiveValue :: o -> m Double

  getTimeout :: m Double
  setTimeout :: Double -> m ()

  optimizeLP :: m SolutionStatus

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'LPMonad' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class LPMonad v c o m => IPMonad v c o m | m -> v c o where
  getDomain :: v -> m Domain
  setDomain :: v -> Domain -> m ()

  getRelativeMIPGap :: m Double
  setRelativeMIPGap :: Double -> m ()

  optimizeIP :: m SolutionStatus

-- | Whether a math program is minimizing or maximizing its objective.
data Sense = Minimization | Maximization
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

-- | The outcome of an optimization.
data SolutionStatus
  = -- | An optimal solution has been found.
    Optimal
  | -- | A feasible solution has been found. The result may or may not
    -- be optimal.
    Feasible
  | -- | The model has been proven to be infeasible.
    Infeasible
  | -- | The model has been proven to be unbounded.
    Unbounded
  | -- | An error was encountered during the solve. Instance-specific
    -- methods should be used to determine what occurred.
    Error
  deriving
    ( Eq,
      Ord,
      Read,
      Show
    )

-- | An interval of the real numbers.
data Bounds
  = -- | The non-negative reals.
    NonNegativeReals
  | -- | The non-positive reals.
    NonPositiveReals
  | -- | Any closed interval of the reals.
    Interval Double Double
  | -- | Any real number.
    Free
  deriving
    ( Read,
      Show
    )

-- | The type of values that a variable can take on.
--
-- Note that the @Integer@ constructor does not interfere with the
-- @Integer@ type, as the @Integer@ type does not define a constuctor
-- of the same name. The ambiguity is unfortunate, but other natural
-- nomenclature such as @Integral@ are similarly conflicted.
data Domain
  = -- | The variable lies in the real numbers
    Continuous
  | -- | The variable lies in the integers
    Integer
  | -- | The variable lies in the set @{0, 1}@.
    Binary
  deriving
    ( Read,
      Show
    )

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
    ( Read,
      Show
    )

-- | Implements addition of 'LinearExpression a b' terms
instance Num a => Semigroup (LinearExpression a b) where
  (LinearExpression termsLhs constantLhs) <> (LinearExpression termsRhs constantRhs) =
    LinearExpression (termsLhs <> termsRhs) (constantLhs + constantRhs)

-- | Using '0' as the identity element
instance Num a => Monoid (LinearExpression a b) where
  mempty = LinearExpression [] 0

instance Functor (LinearExpression a) where
  fmap = fmapDefault

instance Bifunctor LinearExpression where
  first f (LinearExpression terms constant) =
    LinearExpression (fmap (first f) terms) (f constant)
  second f (LinearExpression terms constant) =
    LinearExpression (fmap (fmap f) terms) constant

instance Foldable (LinearExpression a) where
  foldMap = foldMapDefault

-- | Useful for substituting values in a monadic/applicative context
instance Traversable (LinearExpression a) where
  traverse f (LinearExpression terms constant) =
    LinearExpression <$> traverse (traverse f) terms <*> pure constant

-- | Non-strict inequalities.
data Inequality a
  = Inequality Ordering a a
  deriving
    ( Read,
      Show
    )

instance Functor Inequality where
  fmap = fmapDefault

instance Foldable Inequality where
  foldMap = foldMapDefault

instance Traversable Inequality where
  traverse f (Inequality ordering lhs rhs) =
    Inequality ordering <$> f lhs <*> f rhs
