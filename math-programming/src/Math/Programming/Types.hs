{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data and class definitions for the core math programming
-- interface.
module Math.Programming.Types where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text as T
import Math.Programming.LinExpr

-- | A linear program.
--
-- This is a monadic context for formulating and solving linear
-- programs. The types @v@, @c@, and @o@ refer to the types of
-- variables, constraints, and objectives, respectively, used by a
-- particular solver backend.
class Monad m => MonadLP v c o m | m -> v c o where
  -- | Add a new (free) variable to the model.
  --
  -- See 'Math.Programming.Dsl.free', 'Math.Programming.Dsl.bounded',
  -- 'Math.Programming.Dsl.nonNeg', and 'Math.Programming.Dsl.nonPos'
  -- as higher-level alternatives.
  addVariable :: m v

  -- | Remove a variable from the model.
  deleteVariable :: v -> m ()

  -- | Get the name of a variable.
  getVariableName :: v -> m T.Text

  -- | Set a name for a variable.
  setVariableName :: v -> T.Text -> m ()

  -- | Retrieve the current bounds associated with a variable.
  getVariableBounds :: v -> m Bounds

  -- | Apply bounds to a variable.
  --
  -- See 'Math.Programming.Dsl.within' as a higher-level alternative.
  setVariableBounds :: v -> Bounds -> m ()

  -- | Get the value of a variable in the current solution.
  --
  -- This value could be arbitrary if no solve has been completed, or
  -- a solve produced an infeasible or unbounded solution.
  getVariableValue :: v -> m Double

  -- | Add a constraint representing the given inequality to the model.
  --
  -- See the 'Math.Programming.Dsl..==.', 'Math.Programming.Dsl..==#',
  -- 'Math.Programming.Dsl.==.', 'Math.Programming.Dsl..>=.',
  -- 'Math.Programming.Dsl..>=', 'Math.Programming.Dsl.>=.',
  -- 'Math.Programming.Dsl..<=.', 'Math.Programming.Dsl..<=', and
  -- 'Math.Programming.Dsl.<=.' functions as higher-level
  -- alternatives.
  addConstraint :: Inequality (Expr v) -> m c

  -- | Remove a constraint from the model.
  deleteConstraint :: c -> m ()

  -- | Get the name of a constraint.
  getConstraintName :: c -> m T.Text

  -- | Set a name for a constraint.
  setConstraintName :: c -> T.Text -> m ()

  -- | Get the dual value associated with a constraint.
  getConstraintValue :: c -> m Double

  -- | Add an objective to the problem.
  --
  -- Depending on the solver backend, this might replace an existing objective.
  addObjective :: Expr v -> m o

  -- | Remove an objective from the model.
  deleteObjective :: o -> m ()

  -- | Get the name of a objective.
  getObjectiveName :: o -> m T.Text

  -- | Set a name for a objective.
  setObjectiveName :: o -> T.Text -> m ()

  -- | Get the sense of an objective.
  getObjectiveSense :: o -> m Sense

  -- | Set the sense of an objective.
  setObjectiveSense :: o -> Sense -> m ()

  -- | Get the value of an objective.
  getObjectiveValue :: o -> m Double

  -- | Get the timeout associated with a problem.
  getTimeout :: m Double

  -- | Set the timeout associated with a problem.
  setTimeout :: Double -> m ()

  -- | Compute an LP-optimal solution.
  optimizeLP :: m SolutionStatus

-- | Function composition involving a 2-argument function.
compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 = fmap fmap fmap

-- | Monadic lifting  involving a 2-argument function.
lift2 :: (MonadTrans t, Monad m) => (a -> b -> m c) -> (a -> b -> t m c)
lift2 = compose2 lift

instance (MonadLP v c o m) => MonadLP v c o (ReaderT r m) where
  addVariable = lift addVariable
  deleteVariable = lift . deleteVariable
  getVariableName = lift . getVariableName
  setVariableName = lift2 setVariableName
  getVariableValue = lift . getVariableValue
  getVariableBounds = lift . getVariableBounds
  setVariableBounds = lift2 setVariableBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintName = lift . getConstraintName
  setConstraintName = lift2 setConstraintName
  getConstraintValue = lift . getConstraintValue
  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveName = lift . getObjectiveName
  setObjectiveName = lift2 setObjectiveName
  getObjectiveValue = lift . getObjectiveValue
  getObjectiveSense = lift . getObjectiveSense
  setObjectiveSense = lift2 setObjectiveSense
  getTimeout = lift getTimeout
  setTimeout = lift . setTimeout
  optimizeLP = lift optimizeLP

instance MonadLP v c o m => MonadLP v c o (StateT s m) where
  addVariable = lift addVariable
  deleteVariable = lift . deleteVariable
  getVariableValue = lift . getVariableValue
  getVariableName = lift . getVariableName
  setVariableName = lift2 setVariableName
  getVariableBounds = lift . getVariableBounds
  setVariableBounds = lift2 setVariableBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintName = lift . getConstraintName
  setConstraintName = lift2 setConstraintName
  getConstraintValue = lift . getConstraintValue
  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveName = lift . getObjectiveName
  setObjectiveName = lift2 setObjectiveName
  getObjectiveValue = lift . getObjectiveValue
  getObjectiveSense = lift . getObjectiveSense
  setObjectiveSense = lift2 setObjectiveSense
  getTimeout = lift getTimeout
  setTimeout = lift . setTimeout
  optimizeLP = lift optimizeLP

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'MonadLP' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class MonadLP v c o m => MonadIP v c o m | m -> v c o where
  getVariableDomain :: v -> m Domain
  setVariableDomain :: v -> Domain -> m ()

  getRelativeMIPGap :: m Double
  setRelativeMIPGap :: Double -> m ()

  optimizeIP :: m SolutionStatus

instance MonadIP v c o m => MonadIP v c o (ReaderT r m) where
  getVariableDomain = lift . getVariableDomain
  setVariableDomain = lift2 setVariableDomain
  getRelativeMIPGap = lift getRelativeMIPGap
  setRelativeMIPGap = lift . setRelativeMIPGap
  optimizeIP = lift optimizeIP

instance MonadIP v c o m => MonadIP v c o (StateT s m) where
  getVariableDomain = lift . getVariableDomain
  setVariableDomain = lift2 setVariableDomain
  getRelativeMIPGap = lift getRelativeMIPGap
  setRelativeMIPGap = lift . setRelativeMIPGap
  optimizeIP = lift optimizeIP

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

-- | Non-strict inequalities.
data Inequality a
  = Inequality Ordering a a
  deriving
    ( Read,
      Show,
      Functor,
      Foldable,
      Traversable
    )

-- | A convient shorthand for the type of linear expressions used in
-- models.
type Expr = LinExpr Double
