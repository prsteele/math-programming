{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Fuzz testing for math programming backends.
module Math.Programming.Tests.Fuzz where

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import Math.Programming
import System.Random
import System.Random.Stateful
import Test.Hspec hiding (focus, pending)
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype Variable = Variable Int
  deriving
    ( Show,
      Eq,
      Ord
    )

newtype Constraint = Constraint Int
  deriving
    ( Show,
      Eq,
      Ord
    )

newtype Objective = Objective Int
  deriving
    ( Show,
      Eq,
      Ord
    )

-- | The types of actions we can perform on a linear program
data LPAction
  = AddVariable Variable
  | AddThenRemoveVariable Variable
  | AddConstraint Constraint
  | AddThenRemoveConstraint Constraint
  | AddObjective Objective
  | AddThenRemoveObjective Objective
  | Optimize
  deriving (Show)

newtype LPActions = LPActions [LPAction]
  deriving (Show)

instance Arbitrary LPActions where
  arbitrary = do
    NonNegative actionCount <- arbitrary
    actions <- forM [1 .. actionCount] $ \i -> do
      d7 <- fmap (`mod` (7 :: Int)) arbitrary
      pure $ case d7 of
        0 -> AddVariable (Variable i)
        1 -> AddThenRemoveVariable (Variable i)
        2 -> AddConstraint (Constraint i)
        3 -> AddThenRemoveConstraint (Constraint i)
        4 -> AddObjective (Objective i)
        5 -> AddThenRemoveObjective (Objective i)
        _ -> Optimize
    pure (LPActions actions)

data LPState v c o = LPState
  { _variables :: M.Map Variable v,
    _variableNames :: M.Map Variable T.Text,
    _constraints :: M.Map Constraint c,
    _constraintNames :: M.Map Constraint T.Text,
    _objectives :: M.Map Objective o,
    _objectiveNames :: M.Map Objective T.Text,
    _pending :: [LPAction],
    _randomGen :: IOGenM StdGen
  }

makeLenses ''LPState

initLPState :: Int -> [LPAction] -> IO (LPState v c o)
initLPState seed todo = do
  g <- newIOGenM (mkStdGen seed)
  pure
    LPState
      { _variables = M.empty,
        _variableNames = M.empty,
        _constraints = M.empty,
        _constraintNames = M.empty,
        _objectives = M.empty,
        _objectiveNames = M.empty,
        _pending = todo,
        _randomGen = g
      }

type LPFuzz v c o m =
  ( MonadState (LPState v c o) m,
    MonadLP v c o m,
    MonadWriter (S.Seq String) m,
    MonadIO m
  )

evalPending :: LPFuzz v c o m => m ()
evalPending = do
  todo <- use pending
  case todo of
    [] -> pure ()
    (x : xs) -> do
      assign pending xs
      evalAction x
      evalPending

evalAction :: LPFuzz v c o m => LPAction -> m ()
evalAction action = tell (S.singleton (show action)) >> evalAction' action

evalAction' :: LPFuzz v c o m => LPAction -> m ()
evalAction' (AddVariable k) = add k addVariable variables
evalAction' (AddThenRemoveVariable k) = addThenRemove k addVariable deleteVariable variables
evalAction' (AddConstraint k) = add k makeConstraint constraints
evalAction' (AddThenRemoveConstraint k) = addThenRemove k makeConstraint deleteConstraint constraints
evalAction' (AddObjective k) = add k makeObjective objectives
evalAction' (AddThenRemoveObjective k) = addThenRemove k makeObjective deleteObjective objectives
evalAction' Optimize = void optimizeLP

add :: (LPFuzz v c o m, Ord k) => k -> m a -> ASetter' (LPState v c o) (M.Map k a) -> m ()
add k create focus =
  create >>= modifying focus . M.insert k

addThenRemove :: (LPFuzz v c o m, Ord k) => k -> (m a) -> (a -> m ()) -> Lens' (LPState v c o) (M.Map k a) -> m ()
addThenRemove k create destroy focus = do
  collection <- use focus
  case M.lookup k collection of
    Just v -> destroy v >> modifying focus (M.delete k)
    Nothing -> add k create focus

makeConstraint :: LPFuzz v c o m => m c
makeConstraint = do
  lhs <- chooseExpr
  rhs <- chooseExpr
  op <- chooseInequality
  lhs `op` rhs

chooseExpr :: LPFuzz v c o m => m (Expr v)
chooseExpr = do
  g <- use randomGen
  vs <- use variables
  terms <- forM (M.elems vs) $ \v -> do
    c <- liftIO (uniformRM (-1e10, 1e10) g)
    pure (c *. v)

  pure (esum terms)

chooseInequality :: LPFuzz v c o m => m (Expr v -> Expr v -> m c)
chooseInequality = do
  g <- use randomGen
  d3 <- liftIO (uniformRM (0 :: Int, 2) g)
  case d3 of
    0 -> pure (.<=.)
    1 -> pure (.>=.)
    _ -> pure (.==.)

makeObjective :: LPFuzz v c o m => m o
makeObjective = do
  g <- use randomGen
  minimizing <- liftIO (uniformM g)

  if minimizing
    then chooseExpr >>= minimize
    else chooseExpr >>= maximize

makeFuzzTests ::
  (MonadIO m, MonadLP v c o m) =>
  -- | The runner for the API being tested.
  (m (S.Seq String) -> IO ()) ->
  -- | The resulting test suite.
  Spec
makeFuzzTests runner =
  describe "Fuzz testing" $ do
    prop "finds no failures" $ \seed (LPActions todo) -> do
      initState <- liftIO (initLPState seed todo)
      runner . execWriterT
        . flip evalStateT initState
        $ evalPending
