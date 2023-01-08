{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data and class definitions for the core math programming
-- interface.
module Math.Programming.Types where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text as T
import Math.Programming.LinExpr

-- | The class of things that can be named.
--
-- This is used to name variables, constraints, and objectives in
-- linear programs. See also the 'Math.Programming.named' helper function
class Named a m where
  setName :: a -> T.Text -> m ()
  getName :: a -> m T.Text

instance (Monad m, Named a m) => Named a (ReaderT r m) where
  getName = lift . getName
  setName = lift2 setName

instance (Monad m, Named a m) => Named a (StateT r m) where
  getName = lift . getName
  setName = lift2 setName

-- | This instance is really only used for testing.
--
-- In particular,
--
-- @
-- 'setName' x y >> 'getName' x
-- @
--
-- does not necessarily produce the same result as
--
-- @
-- pure y
-- @
instance Named T.Text Identity where
  getName = pure
  setName _ _ = pure ()

-- | A linear program.
--
-- This is a monadic context for formulating and solving linear
-- programs. The types @v@, @c@, and @o@ refer to the types of
-- variables, constraints, and objectives, respectively, used by a
-- particular solver backend.
class (Monad m, Named v m, Named c m, Named o m) => MonadLP v c o m | m -> v c o where
  -- | Add a new (free) variable to the model.
  --
  -- See 'Math.Programming.Dsl.free', 'Math.Programming.Dsl.bounded',
  -- 'Math.Programming.Dsl.nonNeg', and 'Math.Programming.Dsl.nonPos'
  -- as higher-level alternatives.
  addVariable :: m v

  -- | Remove a variable from the model.
  deleteVariable :: v -> m ()

  -- | Retrieve the current bounds associated with a variable.
  getBounds :: v -> m Bounds

  -- | Apply bounds to a variable.
  --
  -- See 'Math.Programming.Dsl.within' as a higher-level alternative.
  setBounds :: v -> Bounds -> m ()

  -- | Get the value of a variable in the current solution.
  --
  -- This value could be arbitrary if no solve has been completed, or
  -- a solve produced an infeasible or unbounded solution.
  getVariableValue :: v -> m Double

  -- | Add a constraint representing the given inequality to the model.
  --
  -- See the 'Math.Programming.Dsl..==.', 'Math.Programming.Dsl..==#',
  -- 'Math.Programming.Dsl.#==.', 'Math.Programming.Dsl..>=.',
  -- 'Math.Programming.Dsl..>=#', 'Math.Programming.Dsl.#>=.',
  -- 'Math.Programming.Dsl..<=.', 'Math.Programming.Dsl..<=#', and
  -- 'Math.Programming.Dsl.#<=.' functions as higher-level
  -- alternatives.
  addConstraint :: Inequality (Expr v) -> m c

  deleteConstraint :: c -> m ()
  getConstraintValue :: c -> m Double

  addObjective :: Expr v -> m o
  deleteObjective :: o -> m ()
  getSense :: o -> m Sense
  setSense :: o -> Sense -> m ()
  getObjectiveValue :: o -> m Double

  getTimeout :: m Double
  setTimeout :: Double -> m ()

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
  getVariableValue = lift . getVariableValue
  getBounds = lift . getBounds
  setBounds = lift2 setBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintValue = lift . getConstraintValue

  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveValue = lift . getObjectiveValue
  getSense = lift . getSense
  setSense = lift2 setSense

  getTimeout = getTimeout
  setTimeout = setTimeout
  optimizeLP = optimizeLP

instance MonadLP v c o m => MonadLP v c o (StateT s m) where
  addVariable = lift addVariable
  deleteVariable = lift . deleteVariable
  getVariableValue = lift . getVariableValue
  getBounds = lift . getBounds
  setBounds = lift2 setBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintValue = lift . getConstraintValue

  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveValue = lift . getObjectiveValue
  getSense = lift . getSense
  setSense = lift2 setSense

  getTimeout = getTimeout
  setTimeout = setTimeout
  optimizeLP = optimizeLP

-- | A (mixed) integer program.
--
-- In addition to the methods of the 'MonadLP' class, this monad
-- supports constraining variables to be either continuous or
-- discrete.
class MonadLP v c o m => MonadIP v c o m | m -> v c o where
  getDomain :: v -> m Domain
  setDomain :: v -> Domain -> m ()

  getRelativeMIPGap :: m Double
  setRelativeMIPGap :: Double -> m ()

  optimizeIP :: m SolutionStatus

instance MonadIP v c o m => MonadIP v c o (ReaderT r m) where
  getDomain = lift . getDomain
  setDomain = lift2 setDomain
  getRelativeMIPGap = lift getRelativeMIPGap
  setRelativeMIPGap = lift . setRelativeMIPGap
  optimizeIP = lift optimizeIP

instance MonadIP v c o m => MonadIP v c o (StateT s m) where
  getDomain = lift . getDomain
  setDomain = lift2 setDomain
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
