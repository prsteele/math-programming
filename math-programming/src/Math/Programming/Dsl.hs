{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Math.Programming.Dsl where

import Data.Functor
import qualified Data.Text as T
import Math.Programming.LinExpr
import Math.Programming.Types
import Text.Printf

-- | Create an objective to be minimized.
minimize :: MonadLP v c o m => Expr v -> m o
minimize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setObjectiveSense objective Minimization
  pure objective

-- | Create an objective to be maximized.
maximize :: MonadLP v c o m => Expr v -> m o
maximize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setObjectiveSense objective Maximization
  pure objective

-- | Get the value of a linear expression in the current solution.
evalExpr :: MonadLP v c o m => Expr v -> m Double
evalExpr expr = traverse getVariableValue expr <&> eval

-- | Create a new free variable.
free :: MonadLP v c o m => m v
free = addVariable `within` Free

-- | Create a new non-negative variable.
nonNeg :: MonadLP v c o m => m v
nonNeg = addVariable `within` NonNegativeReals

-- | Create a new non-positive variable.
nonPos :: MonadLP v c o m => m v
nonPos = addVariable `within` NonPositiveReals

-- | Create a new variable bounded between two values.
bounded :: MonadLP v c o m => Double -> Double -> m v
bounded lo hi = within addVariable (Interval lo hi)

-- | Constrain a variable to take on certain values.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'integer' \``within`\` 'Interval 3 7'
-- @
--
-- creates an integer variable that can take on values 3, 4, 5, 6, or
-- 7.
within :: MonadLP v c o m => m v -> Bounds -> m v
within makeVar bounds = do
  variable <- makeVar
  setVariableBounds variable bounds
  pure variable

-- | Create an integer-valued variable.
integer :: MonadIP v c o m => m v
integer = addVariable `asKind` Integer `within` Free

-- | Create a binary variable.
binary :: MonadIP v c o m => m v
binary = addVariable `asKind` Binary

-- | Create an integer-value variable that takes on non-negative values.
nonNegInteger :: MonadIP v c o m => m v
nonNegInteger = addVariable `asKind` Integer `within` NonNegativeReals

-- | Create an integer-value variable that takes on non-positive values.
nonPosInteger :: MonadIP v c o m => m v
nonPosInteger = addVariable `asKind` Integer `within` NonPositiveReals

-- | Set the type of a variable.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'free' \``asKind`\` 'Binary'
-- @
asKind :: MonadIP v c o m => m v -> Domain -> m v
asKind make dom = do
  variable <- make
  setVariableDomain variable dom
  pure variable

-- | A less-than or equal-to constraint
(.<=.) :: MonadLP v c o m => Expr v -> Expr v -> m c
(.<=.) x y = addConstraint $ Inequality LT x y

-- | A less-than or equal-to constraint with a numeric left-hand side
(<=.) :: MonadLP v c o m => Double -> Expr v -> m c
(<=.) x y = con x .<=. y

-- | A less-than or equal-to constraint with a numeric right-hand side
(.<=) :: MonadLP v c o m => Expr v -> Double -> m c
(.<=) x y = x .<=. con y

-- | A greater-than or equal-to constraint
(.>=.) :: MonadLP v c o m => Expr v -> Expr v -> m c
(.>=.) x y = addConstraint $ Inequality GT x y

-- | A greater-than or equal-to constraint with a numeric left-hand side
(>=.) :: MonadLP v c o m => Double -> Expr v -> m c
(>=.) x y = con x .>=. y

-- | A greater-than or equal-to constraint with a numeric right-hand side
(.>=) :: MonadLP v c o m => Expr v -> Double -> m c
(.>=) x y = x .>=. con y

-- | An equality constraint
(.==.) :: MonadLP v c o m => Expr v -> Expr v -> m c
(.==.) x y = addConstraint $ Inequality EQ x y

-- | An equality constraint with a numeric left-hand side
(==.) :: MonadLP v c o m => Double -> Expr v -> m c
(==.) x y = con x .==. y

-- | An equality constraint with a numeric right-hand side
(.==) :: MonadLP v c o m => Expr v -> Double -> m c
(.==) x y = x .==. con y

infix 4 <=.

infix 4 .<=

infix 4 .<=.

infix 4 >=.

infix 4 .>=

infix 4 .>=.

infix 4 ==.

infix 4 .==

infix 4 .==.

formatExpr :: MonadLP v c o m => Expr v -> m T.Text
formatExpr = formatExpr' getVariableName

formatExpr' :: Monad m => (v -> m T.Text) -> Expr v -> m T.Text
formatExpr' nameOf (LinExpr terms coef) = do
  names <- mapM (traverse nameOf) terms
  let strTerms = fmap (T.pack . uncurry (printf "%f * %s")) names
  pure $ T.intercalate " + " (strTerms <> [T.pack (show coef)])

withVariableName :: MonadLP v c o m => m v -> T.Text -> m v
withVariableName mv name = do
  v <- mv
  setVariableName v name
  pure v

withConstraintName :: MonadLP v c o m => m c -> T.Text -> m c
withConstraintName mc name = do
  c <- mc
  setConstraintName c name
  pure c

withObjectiveName :: MonadLP v c o m => m o -> T.Text -> m o
withObjectiveName mo name = do
  o <- mo
  setObjectiveName o name
  pure o
