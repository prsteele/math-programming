{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Programming.Dsl where

import Data.Bifunctor
import Data.Functor
import Data.List (sortOn)
import qualified Data.Text as T
import Math.Programming.Types

-- | Create an objective to be minimized.
minimize :: LPMonad v c o m => Expr v -> m o
minimize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setSense objective Minimization
  pure objective

-- | Create an objective to be maximized.
maximize :: LPMonad v c o m => Expr v -> m o
maximize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setSense objective Maximization
  pure objective

-- | Get the value of a linear expression in the current solution.
evalExpr :: LPMonad v c o m => Expr v -> m Double
evalExpr expr = traverse getVariableValue expr <&> eval

-- | Create a new free variable.
free :: LPMonad v c o m => m v
free = addVariable `within` Free

-- | Create a new non-negative variable.
nonNeg :: LPMonad v c o m => m v
nonNeg = addVariable `within` NonNegativeReals

-- | Create a new non-positive variable.
nonPos :: LPMonad v c o m => m v
nonPos = addVariable `within` NonPositiveReals

-- | Create a new variable bounded between two values.
bounded :: LPMonad v c o m => Double -> Double -> m v
bounded lo hi = within addVariable (Interval lo hi)

-- | Constrain a variable to take on certain values.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'create' \``within`\` 'NonNegativeReals'
-- @
within :: LPMonad v c o m => m v -> Bounds -> m v
within makeVar bounds = do
  variable <- makeVar
  setBounds variable bounds
  pure variable

-- | Create an integer-valued variable.
integer :: IPMonad v c o m => m v
integer = addVariable `asKind` Integer `within` Free

-- | Create a binary variable.
binary :: IPMonad v c o m => m v
binary = addVariable `asKind` Binary

-- | Create an integer-value variable that takes on non-negative values.
nonNegInteger :: IPMonad v c o m => m v
nonNegInteger = addVariable `asKind` Integer `within` NonNegativeReals

-- | Create an integer-value variable that takes on non-positive values.
nonPosInteger :: IPMonad v c o m => m v
nonPosInteger = addVariable `asKind` Integer `within` NonPositiveReals

-- | Set the type of a variable.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'create' \``asKind`\` 'Binary'
-- @
asKind :: IPMonad v c o m => m v -> Domain -> m v
asKind make dom = do
  variable <- make
  setDomain variable dom
  pure variable

-- -- | Name a variable, constraint, or objective.
-- --
-- -- This function is designed to be used as an infix operator, e.g.
-- --
-- -- @
-- -- 'free' \``named`\` "X_1"
-- -- @
-- named :: (Named v c o m a) => m a -> T.Text -> m a
-- named make n = do
--   x <- make
--   setName x n
--   pure x

(#+@) :: Num a => a -> b -> LinearExpression a b
(#+.) :: Num a => a -> LinearExpression a b -> LinearExpression a b
(@+#) :: Num a => b -> a -> LinearExpression a b
(@+@) :: Num a => b -> b -> LinearExpression a b
(@+.) :: Num a => b -> LinearExpression a b -> LinearExpression a b
(.+#) :: Num a => LinearExpression a b -> a -> LinearExpression a b
(.+@) :: Num a => LinearExpression a b -> b -> LinearExpression a b
(.+.) :: Num a => LinearExpression a b -> LinearExpression a b -> LinearExpression a b
(#-@) :: Num a => a -> b -> LinearExpression a b
(#-.) :: Num a => a -> LinearExpression a b -> LinearExpression a b
(@-#) :: Num a => b -> a -> LinearExpression a b
(@-@) :: Num a => b -> b -> LinearExpression a b
(@-.) :: Num a => b -> LinearExpression a b -> LinearExpression a b
(.-#) :: Num a => LinearExpression a b -> a -> LinearExpression a b
(.-@) :: Num a => LinearExpression a b -> b -> LinearExpression a b
(.-.) :: Num a => LinearExpression a b -> LinearExpression a b -> LinearExpression a b
(#*.) :: Num a => a -> LinearExpression a b -> LinearExpression a b
(.*#) :: Num a => LinearExpression a b -> a -> LinearExpression a b
(#*@) :: Num a => a -> b -> LinearExpression a b
(@*#) :: Num a => b -> a -> LinearExpression a b
(@/#) :: Fractional a => b -> a -> LinearExpression a b
(./#) :: Fractional a => LinearExpression a b -> a -> LinearExpression a b
x #+@ y = con x .+. var y

x #+. y = con x .+. y

x @+# y = var x .+. con y

x @+@ y = var x .+. var y

x @+. y = var x .+. y

x .+@ y = x .+. var y

x .+# y = x .+. con y

x .+. y = x <> y

x #-@ y = con x .-. var y

x #-. y = con x .-. y

x @-# y = var x .-. con y

x @-@ y = var x .-. var y

x @-. y = var x .-. y

x .-# y = x .-. con y

x .-@ y = x .-. var y

x .-. y = x .+. (-1) #*. y

x #*@ y = var y .*# x

x #*. y = y .*# x

x @*# y = var x .*# y

x .*# y = first (* y) x

x @/# y = var x ./# y

x ./# y = first (/ y) x

infixl 6 #+@

infixl 6 #+.

infixl 6 @+#

infixl 6 @+@

infixl 6 @+.

infixl 6 .+#

infixl 6 .+@

infixl 6 .+.

infixl 6 #-@

infixl 6 #-.

infixl 6 @-#

infixl 6 @-@

infixl 6 @-.

infixl 6 .-#

infixl 6 .-@

infixl 6 .-.

infixl 7 #*@

infixl 7 #*.

infixl 7 @*#

infixl 7 .*#

infixl 7 @/#

infixl 7 ./#

-- | Combine equivalent terms by summing their coefficients.
simplify :: (Ord b, Num a) => LinearExpression a b -> LinearExpression a b
simplify (LinearExpression terms constant) =
  LinearExpression (reduce (sortOn snd terms)) constant
  where
    reduce [] = []
    reduce [(c, x)] = [(c, x)]
    reduce ((c, x) : (c', x') : xs)
      | x == x' = reduce $ (c + c', x) : xs
      | otherwise = (c, x) : reduce ((c', x') : xs)

-- | Reduce an expression to its value.
eval :: Num a => LinearExpression a a -> a
eval (LinearExpression terms constant) = constant + sum (map (uncurry (*)) terms)

-- | Construct an expression representing a variable.
var :: Num a => b -> LinearExpression a b
var x = LinearExpression [(1, x)] 0

-- | Construct an expression representing a constant.
con :: Num a => a -> LinearExpression a b
con = LinearExpression []

-- | Construct an expression by summing expressions.
exprSum :: Num a => [LinearExpression a b] -> LinearExpression a b
exprSum = mconcat

-- | Construct an expression by summing variables.
varSum :: [b] -> LinearExpression Double b
varSum = mconcat . fmap var

(#<=@) :: LPMonad v c o m => Double -> v -> m c
(#<=.) :: LPMonad v c o m => Double -> Expr v -> m c
(@<=#) :: LPMonad v c o m => v -> Double -> m c
(@<=@) :: LPMonad v c o m => v -> v -> m c
(@<=.) :: LPMonad v c o m => v -> Expr v -> m c
(.<=#) :: LPMonad v c o m => Expr v -> Double -> m c
(.<=@) :: LPMonad v c o m => Expr v -> v -> m c
(.<=.) :: LPMonad v c o m => Expr v -> Expr v -> m c
(#>=@) :: LPMonad v c o m => Double -> v -> m c
(#>=.) :: LPMonad v c o m => Double -> Expr v -> m c
(@>=#) :: LPMonad v c o m => v -> Double -> m c
(@>=@) :: LPMonad v c o m => v -> v -> m c
(@>=.) :: LPMonad v c o m => v -> Expr v -> m c
(.>=#) :: LPMonad v c o m => Expr v -> Double -> m c
(.>=@) :: LPMonad v c o m => Expr v -> v -> m c
(.>=.) :: LPMonad v c o m => Expr v -> Expr v -> m c
(#==@) :: LPMonad v c o m => Double -> v -> m c
(#==.) :: LPMonad v c o m => Double -> Expr v -> m c
(@==#) :: LPMonad v c o m => v -> Double -> m c
(@==@) :: LPMonad v c o m => v -> v -> m c
(@==.) :: LPMonad v c o m => v -> Expr v -> m c
(.==#) :: LPMonad v c o m => Expr v -> Double -> m c
(.==@) :: LPMonad v c o m => Expr v -> v -> m c
(.==.) :: LPMonad v c o m => Expr v -> Expr v -> m c
x #<=@ y = addConstraint $ Inequality LT (con x) (var y)

x #<=. y = addConstraint $ Inequality LT (con x) y

x @<=# y = addConstraint $ Inequality LT (var x) (con y)

x @<=@ y = addConstraint $ Inequality LT (var x) (var y)

x @<=. y = addConstraint $ Inequality LT (var x) y

x .<=# y = addConstraint $ Inequality LT x (con y)

x .<=@ y = addConstraint $ Inequality LT x (var y)

x .<=. y = addConstraint $ Inequality LT x y

x #>=@ y = addConstraint $ Inequality GT (con x) (var y)

x #>=. y = addConstraint $ Inequality GT (con x) y

x @>=# y = addConstraint $ Inequality GT (var x) (con y)

x @>=@ y = addConstraint $ Inequality GT (var x) (var y)

x @>=. y = addConstraint $ Inequality GT (var x) y

x .>=# y = addConstraint $ Inequality GT x (con y)

x .>=@ y = addConstraint $ Inequality GT x (var y)

x .>=. y = addConstraint $ Inequality GT x y

x #==@ y = addConstraint $ Inequality EQ (con x) (var y)

x #==. y = addConstraint $ Inequality EQ (con x) y

x @==# y = addConstraint $ Inequality EQ (var x) (con y)

x @==@ y = addConstraint $ Inequality EQ (var x) (var y)

x @==. y = addConstraint $ Inequality EQ (var x) y

x .==# y = addConstraint $ Inequality EQ x (con y)

x .==@ y = addConstraint $ Inequality EQ x (var y)

x .==. y = addConstraint $ Inequality EQ x y

infix 4 #<=@

infix 4 #<=.

infix 4 @<=#

infix 4 @<=@

infix 4 @<=.

infix 4 .<=#

infix 4 .<=@

infix 4 .<=.

infix 4 #>=@

infix 4 #>=.

infix 4 @>=#

infix 4 @>=@

infix 4 @>=.

infix 4 .>=#

infix 4 .>=@

infix 4 .>=.

infix 4 #==@

infix 4 #==.

infix 4 @==#

infix 4 @==@

infix 4 @==.

infix 4 .==#

infix 4 .==@

infix 4 .==.
