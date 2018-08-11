module Math.Programming where

newtype Variable = Variable Int

newtype Constraint = Constraint Int

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

class Monad m => LP m where
  makeVariable :: m Variable
  makeConstraint :: m Constraint
  setSense :: Sense -> m ()
  optimize :: m SolutionStatus
