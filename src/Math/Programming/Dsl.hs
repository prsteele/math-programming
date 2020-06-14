module Math.Programming.Dsl where

import           Data.Bifunctor
import           Data.List              (sortOn)

import           Math.Programming.Types

-- | Create an objective to be minimized.
minimize :: LPMonad m => Expr m -> m (Objective m)
minimize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setObjectiveSense objective Minimization
  pure objective

-- | Create an objective to be maximized.
maximize :: LPMonad m => Expr m -> m (Objective m)
maximize objectiveExpr = do
  objective <- addObjective objectiveExpr
  setObjectiveSense objective Maximization
  pure objective

-- | Get the value of a linear expression in the current solution.
evalExpr :: LPMonad m => Expr m -> m (Numeric m)
evalExpr expr = traverse getVariableValue expr >>= return . eval

-- | Create a new free variable.
free :: LPMonad m => m (Variable m)
free = addVariable `within` Free

-- | Create a new non-negative variable.
nonNeg :: LPMonad m => m (Variable m)
nonNeg = addVariable `within` NonNegativeReals

-- | Create a new non-positive variable.
nonPos :: LPMonad m => m (Variable m)
nonPos = addVariable `within` NonPositiveReals

-- | Create a new variable bounded between two values.
bounded :: LPMonad m => Numeric m -> Numeric m -> m (Variable m)
bounded lo hi = within addVariable (Interval lo hi)

-- | Constrain a variable to take on certain values.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'addVariable' \``within`\` 'NonNegativeReals'
-- @
within :: LPMonad m => m (Variable m) -> Bounds (Numeric m) -> m (Variable m)
within makeVar bounds = do
  variable <- makeVar
  setVariableBounds variable bounds
  pure variable

-- | Create an integer-valued variable.
integer :: IPMonad m => m (Variable m)
integer = addVariable `asKind` Integer

-- | Create a binary variable.
binary :: IPMonad m => m (Variable m)
binary = addVariable `asKind` Binary

-- | Create an integer-value variable that takes on non-negative values.
nonNegInteger :: IPMonad m => m (Variable m)
nonNegInteger = addVariable `asKind` Integer `within` NonNegativeReals

-- | Create an integer-value variable that takes on non-positive values.
nonPosInteger :: IPMonad m => m (Variable m)
nonPosInteger = addVariable `asKind` Integer `within` NonPositiveReals

-- | Set the type of a variable.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'addVariable' \``asKind`\` 'Binary'
-- @
asKind :: IPMonad m => m (Variable m) -> Domain -> m (Variable m)
asKind make domain = do
  variable <- make
  setVariableDomain variable domain
  pure variable

-- | Name a variable, constraint, or objective.
--
-- This function is designed to be used as an infix operator, e.g.
--
-- @
-- 'free' \``named`\` "X_1"
-- @
named :: (Monad m, Nameable m a) => m a -> String -> m a
named make name = do
  x <- make
  setName x name
  pure x

-- | Retrieve the name of a variable, constraint, or objective.
nameOf :: (Monad m, Nameable m a) => a -> m String
nameOf = getName

(#+@) :: Num a => a                    -> b                    -> LinearExpression a b
(#+.) :: Num a => a                    -> LinearExpression a b -> LinearExpression a b
(@+#) :: Num a => b                    -> a                    -> LinearExpression a b
(@+@) :: Num a => b                    -> b                    -> LinearExpression a b
(@+.) :: Num a => b                    -> LinearExpression a b -> LinearExpression a b
(.+#) :: Num a => LinearExpression a b -> a                    -> LinearExpression a b
(.+@) :: Num a => LinearExpression a b -> b                    -> LinearExpression a b
(.+.) :: Num a => LinearExpression a b -> LinearExpression a b -> LinearExpression a b
(#-@) :: Num a => a                    -> b                    -> LinearExpression a b
(#-.) :: Num a => a                    -> LinearExpression a b -> LinearExpression a b
(@-#) :: Num a => b                    -> a                    -> LinearExpression a b
(@-@) :: Num a => b                    -> b                    -> LinearExpression a b
(@-.) :: Num a => b                    -> LinearExpression a b -> LinearExpression a b
(.-#) :: Num a => LinearExpression a b -> a                    -> LinearExpression a b
(.-@) :: Num a => LinearExpression a b -> b                    -> LinearExpression a b
(.-.) :: Num a => LinearExpression a b -> LinearExpression a b -> LinearExpression a b
(#*.) :: Num a => a                    -> LinearExpression a b -> LinearExpression a b
(.*#) :: Num a => LinearExpression a b -> a                    -> LinearExpression a b
(#*@) :: Num a => a                    -> b                    -> LinearExpression a b
(@*#) :: Num a => b                    -> a                    -> LinearExpression a b
(@/#) :: Fractional a => b -> a -> LinearExpression a b
(./#) :: Fractional a => LinearExpression a b -> a -> LinearExpression a b

x #+@ y = con x .+. var y
x #+. y = con x .+. y
x @+# y = var x .+. con y
x @+@ y = var x .+. var y
x @+. y = var x .+. y
x .+@ y = x     .+. var y
x .+# y = x     .+. con y
x .+. y = x     <>  y
x #-@ y = con x .-. var y
x #-. y = con x .-. y
x @-# y = var x .-. con y
x @-@ y = var x .-. var y
x @-. y = var x .-. y
x .-# y = x     .-. con y
x .-@ y = x     .-. var y
x .-. y = x     .+. (-1) #*. y
x #*@ y = var y .*# x
x #*. y = y     .*# x
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
simplify (LinearExpression terms constant)
  = LinearExpression (reduce (sortOn snd terms)) constant
  where
    reduce []           = []
    reduce ((c, x): []) = [(c, x)]
    reduce ((c, x): (c', x'): xs)
      | x == x'   = (c + c', x) : reduce xs
      | otherwise = (c, x) : reduce ((c', x'): xs)

-- | Reduce an expression to its value.
eval :: Num a => LinearExpression a a -> a
eval (LinearExpression terms constant) = constant + sum (map (uncurry (*)) terms)

-- | Construct an expression representing a variable.
var :: Num a => b -> LinearExpression a b
var x = LinearExpression [(1, x)] 0

-- | Construct an expression representing a constant.
con :: Num a => a -> LinearExpression a b
con x = LinearExpression [] x

-- | Construct an expression by summing expressions.
exprSum :: Num a => [LinearExpression a b] -> LinearExpression a b
exprSum = mconcat

-- | Construct an expression by summing variables.
varSum :: Num a => [b] -> LinearExpression a b
varSum = mconcat . fmap var

(#<=@) :: LPMonad m => Numeric m  -> Variable m -> m (Constraint m)
(#<=.) :: LPMonad m => Numeric m  -> Expr m     -> m (Constraint m)
(@<=#) :: LPMonad m => Variable m -> Numeric m  -> m (Constraint m)
(@<=@) :: LPMonad m => Variable m -> Variable m -> m (Constraint m)
(@<=.) :: LPMonad m => Variable m -> Expr m     -> m (Constraint m)
(.<=#) :: LPMonad m => Expr m     -> Numeric m  -> m (Constraint m)
(.<=@) :: LPMonad m => Expr m     -> Variable m -> m (Constraint m)
(.<=.) :: LPMonad m => Expr m     -> Expr m     -> m (Constraint m)
(#>=@) :: LPMonad m => Numeric m  -> Variable m -> m (Constraint m)
(#>=.) :: LPMonad m => Numeric m  -> Expr m     -> m (Constraint m)
(@>=#) :: LPMonad m => Variable m -> Numeric m  -> m (Constraint m)
(@>=@) :: LPMonad m => Variable m -> Variable m -> m (Constraint m)
(@>=.) :: LPMonad m => Variable m -> Expr m     -> m (Constraint m)
(.>=#) :: LPMonad m => Expr m     -> Numeric m  -> m (Constraint m)
(.>=@) :: LPMonad m => Expr m     -> Variable m -> m (Constraint m)
(.>=.) :: LPMonad m => Expr m     -> Expr m     -> m (Constraint m)
(#==@) :: LPMonad m => Numeric m  -> Variable m -> m (Constraint m)
(#==.) :: LPMonad m => Numeric m  -> Expr m     -> m (Constraint m)
(@==#) :: LPMonad m => Variable m -> Numeric m  -> m (Constraint m)
(@==@) :: LPMonad m => Variable m -> Variable m -> m (Constraint m)
(@==.) :: LPMonad m => Variable m -> Expr m     -> m (Constraint m)
(.==#) :: LPMonad m => Expr m     -> Numeric m  -> m (Constraint m)
(.==@) :: LPMonad m => Expr m     -> Variable m -> m (Constraint m)
(.==.) :: LPMonad m => Expr m     -> Expr m     -> m (Constraint m)

x #<=@ y = addConstraint $ Inequality LT (con x) (var y)
x #<=. y = addConstraint $ Inequality LT (con x) y
x @<=# y = addConstraint $ Inequality LT (var x) (con y)
x @<=@ y = addConstraint $ Inequality LT (var x) (var y)
x @<=. y = addConstraint $ Inequality LT (var x) y
x .<=# y = addConstraint $ Inequality LT x       (con y)
x .<=@ y = addConstraint $ Inequality LT x       (var y)
x .<=. y = addConstraint $ Inequality LT x       y
x #>=@ y = addConstraint $ Inequality GT (con x) (var y)
x #>=. y = addConstraint $ Inequality GT (con x) y
x @>=# y = addConstraint $ Inequality GT (var x) (con y)
x @>=@ y = addConstraint $ Inequality GT (var x) (var y)
x @>=. y = addConstraint $ Inequality GT (var x) y
x .>=# y = addConstraint $ Inequality GT x       (con y)
x .>=@ y = addConstraint $ Inequality GT x       (var y)
x .>=. y = addConstraint $ Inequality GT x       y
x #==@ y = addConstraint $ Inequality EQ (con x) (var y)
x #==. y = addConstraint $ Inequality EQ (con x) y
x @==# y = addConstraint $ Inequality EQ (var x) (con y)
x @==@ y = addConstraint $ Inequality EQ (var x) (var y)
x @==. y = addConstraint $ Inequality EQ (var x) y
x .==# y = addConstraint $ Inequality EQ x       (con y)
x .==@ y = addConstraint $ Inequality EQ x       (var y)
x .==. y = addConstraint $ Inequality EQ x       y

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
