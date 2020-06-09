{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Math.Programming where

import           Data.List        (sortOn)
import           Data.Traversable (fmapDefault, foldMapDefault)


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
  addConstraint :: Inequality (Expr m) -> m (Constraint m)

  -- | Associate a name with a constraint.
  setConstraintName :: Constraint m -> String -> m ()

  -- Retrieve the name of the constraint.
  getConstraintName :: Constraint m -> m String

  -- | Delete a constraint from the model.
  --
  -- The constraint cannot used after being deleted.
  deleteConstraint :: Constraint m -> m ()

  -- | Set the objective function of the model.
  setObjective :: Expr m -> m ()

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
  evalExpr :: Expr m -> m (Numeric m)
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

-- | Expressions in the LPMonad m.
type Expr m = LinearExpr (Numeric m) (Variable m)

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

-- | A linear expression containing symbolic variables of type @b@ and
-- numeric coefficients of type @a@.
--
-- Using strings to denote variables and 'Double's as our numeric
-- type, we could express the term /3 x + 2 y + 1/ as
--
-- @
--   LinearExpr [(3, "x"), (2, "y")] 1
-- @
data LinearExpr a b
  = LinearExpr
    { _terms    :: [(a, b)]
    , _constant :: a
    }
  deriving
    ( Read
    , Show
    )

instance (Num a) => Semigroup (LinearExpr a b) where
  (LinearExpr termsLhs constantLhs) <> (LinearExpr termsRhs constantRhs)
    = LinearExpr (termsLhs <> termsRhs) (constantLhs + constantRhs)

instance (Num a) => Monoid (LinearExpr a b) where
  mempty = LinearExpr [] 0

instance Functor (LinearExpr a) where
  fmap = fmapDefault

instance Foldable (LinearExpr a) where
  foldMap = foldMapDefault

instance Traversable (LinearExpr a) where
  traverse f (LinearExpr terms constant)
    = LinearExpr <$> traverse (traverse f) terms <*> pure constant

-- | Non-strict inequalities.
data Inequality a
  = Inequality Ordering a a
  deriving
    ( Read
    , Show
    )

(.+) :: Num a => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
(LinearExpr terms constant) .+ (LinearExpr terms' constant')
  = LinearExpr (terms <> terms') (constant + constant')
infixl 6 .+

(.-) :: Num a => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
x .- y = x .+ scale (-1) y
infixl 6 .-

scale :: Num a => a -> LinearExpr a b -> LinearExpr a b
scale x (LinearExpr terms constant) = LinearExpr terms' constant'
  where
    terms' = (\(coef, symbol) -> (coef * x, symbol)) <$> terms
    constant' = x * constant

(.*) :: Num a => a -> b -> LinearExpr a b
x .* y = LinearExpr [(x, y)] 0
infixl 6 .*

(./) :: Fractional a => b -> a -> LinearExpr a b
x ./ y = LinearExpr [(1 / y, x)] 0
infixl 6 ./

sumExpr :: Num a => [LinearExpr a b] -> LinearExpr a b
sumExpr = mconcat

-- | Combine equivalent terms by summing their coefficients
simplify :: (Ord b, Num a) => LinearExpr a b -> LinearExpr a b
simplify (LinearExpr terms constant)
  = LinearExpr (reduce (sortOn snd terms)) constant
  where
    reduce []           = []
    reduce ((c, x): []) = [(c, x)]
    reduce ((c, x): (c', x'): xs)
      | x == x'   = (c + c', x) : reduce xs
      | otherwise = (c, x) : reduce ((c', x'): xs)

eval :: Num a => LinearExpr a a -> a
eval (LinearExpr terms constant) = constant + sum (map (uncurry (*)) terms)

constantExpr :: a -> LinearExpr a b
constantExpr = LinearExpr []

zero :: Num a => LinearExpr a b
zero = constantExpr 0

one :: Num a => LinearExpr a b
one = constantExpr 1

(.<=) :: Expr m -> Expr m -> Inequality (Expr m)
(.<=) = Inequality LT
infix 4 .<=

(.<=#) :: Expr m -> Numeric m -> Inequality (Expr m)
x .<=# y = Inequality LT x (constantExpr y)
infix 4 .<=#

(.>=) :: Expr m -> Expr m -> Inequality (Expr m)
(.>=) = Inequality GT
infix 4 .>=

(#<=.) :: Numeric m -> Expr m -> Inequality (Expr m)
(#<=.) = flip (.>=#)
infix 4 #<=.

(.>=#) :: Expr m -> Numeric m -> Inequality (Expr m)
x .>=# y = Inequality GT x (constantExpr y)
infix 4 .>=#

(#>=.) :: Numeric m -> Expr m -> Inequality (Expr m)
(#>=.) = flip (.>=#)
infix 4 #>=.


(.==) :: Expr m -> Expr m -> Inequality (Expr m)
(.==) = Inequality LT
infix 4 .==

(.==#) :: Expr m -> Numeric m -> Inequality (Expr m)
x .==# y = Inequality LT x (constantExpr y)
infix 4 .==#

(#==.) :: Numeric m -> Expr m -> Inequality (Expr m)
(#==.) = flip (.==#)
infix 4 #==.
