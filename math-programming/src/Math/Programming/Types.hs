{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Programming.Types where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Text as T
import Math.Programming.LinExpr

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

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 = fmap fmap fmap

lift2 :: (MonadTrans t, Monad m) => (a -> b -> m c) -> (a -> b -> t m c)
lift2 = compose2 lift

instance LPMonad v c o m => LPMonad v c o (ReaderT r m) where
  addVariable = lift addVariable
  deleteVariable = lift . deleteVariable
  getVariableName = lift . getVariableName
  setVariableName = lift2 setVariableName
  getVariableValue = lift . getVariableValue
  getBounds = lift . getBounds
  setBounds = lift2 setBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintName = lift . getConstraintName
  setConstraintName = lift2 setConstraintName
  getConstraintValue = lift . getConstraintValue

  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveValue = lift . getObjectiveValue
  getSense = lift . getSense
  setSense = lift2 setSense
  getObjectiveName = lift . getObjectiveName
  setObjectiveName = lift2 setObjectiveName

  getTimeout = getTimeout
  setTimeout = setTimeout
  optimizeLP = optimizeLP

instance LPMonad v c o m => LPMonad v c o (StateT s m) where
  addVariable = lift addVariable
  deleteVariable = lift . deleteVariable
  getVariableName = lift . getVariableName
  setVariableName = lift2 setVariableName
  getVariableValue = lift . getVariableValue
  getBounds = lift . getBounds
  setBounds = lift2 setBounds
  addConstraint = lift . addConstraint
  deleteConstraint = lift . deleteConstraint
  getConstraintName = lift . getConstraintName
  setConstraintName = lift2 setConstraintName
  getConstraintValue = lift . getConstraintValue

  addObjective = lift . addObjective
  deleteObjective = lift . deleteObjective
  getObjectiveValue = lift . getObjectiveValue
  getSense = lift . getSense
  setSense = lift2 setSense
  getObjectiveName = lift . getObjectiveName
  setObjectiveName = lift2 setObjectiveName

  getTimeout = getTimeout
  setTimeout = setTimeout
  optimizeLP = optimizeLP

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

instance IPMonad v c o m => IPMonad v c o (ReaderT r m) where
  getDomain = lift . getDomain
  setDomain = lift2 setDomain
  getRelativeMIPGap = lift getRelativeMIPGap
  setRelativeMIPGap = lift . setRelativeMIPGap
  optimizeIP = lift optimizeIP

instance IPMonad v c o m => IPMonad v c o (StateT s m) where
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

-- | A convient shorthand for the type of linear expressions used in a
-- given model.
type Expr = LinExpr Double
