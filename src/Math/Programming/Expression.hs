{-| Building and manipulating symbolic linear expressions.

This module defines a variety of operators to build linear expressions
depending on whether the operands are constants, variables, or
expressions. (The results are always expressions.) When defining these
operators we follow the convention that @#@ abut constants, @\@@ abut
variables, and @.@ abut expressions. For example, '#*@' multiplies a
constant to a variable, while '.+.' sums two expressions.
-}
module Math.Programming.Expression
  ( -- * Construction
    LinearExpr (..)
  , var
  , con
  , exprSum
  , varSum
    -- * Arithmetic
    -- ** Addition
    -- $addition
  , (.+.)
  , (@+@)
  , (.+@)
  , (@+.)
  , (@+#)
  , (#+@)
  , (#+.)
  , (.+#)
    -- ** Subtraction
    -- $subtraction
  , (.-.)
  , (@-@)
  , (.-@)
  , (@-.)
  , (@-#)
  , (#-@)
  , (#-.)
  , (.-#)
    -- ** Multiplication
    -- $multiplication
  , (#*@)
  , (@*#)
  , (#*.)
  , (.*#)
    -- ** Division
    -- $division
  , (@/#)
  , (./#)
    -- * Evaluation
  , simplify
  , eval
  ) where

import           Data.Bifunctor
import           Data.List        (sortOn)
import           Data.Traversable (fmapDefault, foldMapDefault)

-- | A linear expression containing symbolic variables of type @b@ and
-- numeric coefficients of type @a@.
--
-- Using 'String's to denote variables and 'Double's as our numeric
-- type, we could express /3 x + 2 y + 1/ as
--
-- @
--   LinearExpr [(3, "x"), (2, "y")] 1
-- @
data LinearExpr a b
  = LinearExpr [(a, b)] a
  deriving
    ( Read
    , Show
    )

-- | Implements addition of 'LinearExpr a b' terms
instance Num a => Semigroup (LinearExpr a b) where
  (LinearExpr termsLhs constantLhs) <> (LinearExpr termsRhs constantRhs)
    = LinearExpr (termsLhs <> termsRhs) (constantLhs + constantRhs)

-- | Using '0' as the identity element
instance Num a => Monoid (LinearExpr a b) where
  mempty = con 0

instance Functor (LinearExpr a) where
  fmap = fmapDefault

instance Bifunctor LinearExpr where
  first f (LinearExpr terms constant)
    = LinearExpr (fmap (first f) terms) (f constant)
  second f (LinearExpr terms constant)
    = LinearExpr (fmap (fmap f) terms) constant

instance Foldable (LinearExpr a) where
  foldMap = foldMapDefault

-- | Useful for substituting values in a monadic/applicative context
instance Traversable (LinearExpr a) where
  traverse f (LinearExpr terms constant)
    = LinearExpr <$> traverse (traverse f) terms <*> pure constant

-- | Construct an expression representing a variable.
var :: Num a => b -> LinearExpr a b
var x = LinearExpr [(1, x)] 0

-- | Construct an expression representing a constant.
con :: Num a => a -> LinearExpr a b
con x = LinearExpr [] x

-- | Construct an expression by summing expressions.
exprSum :: Num a => [LinearExpr a b] -> LinearExpr a b
exprSum = mconcat

-- | Construct an expression by summing variables.
varSum :: Num a => [b] -> LinearExpr a b
varSum = mconcat . fmap var

-- $addition
--
-- We can summarize the addition operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '+'    | '#+@'  | '#+.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@+#'  | '@+@'  | '@+.'            |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.+#'  | '.+@'  | '.+.'            |
-- +-----------------+--------+--------+------------------+

(#+@) :: Num a => a              -> b              -> LinearExpr a b
(#+.) :: Num a => a              -> LinearExpr a b -> LinearExpr a b
(@+#) :: Num a => b              -> a              -> LinearExpr a b
(@+@) :: Num a => b              -> b              -> LinearExpr a b
(@+.) :: Num a => b              -> LinearExpr a b -> LinearExpr a b
(.+#) :: Num a => LinearExpr a b -> a              -> LinearExpr a b
(.+@) :: Num a => LinearExpr a b -> b              -> LinearExpr a b
(.+.) :: Num a => LinearExpr a b -> LinearExpr a b -> LinearExpr a b

x #+@ y = con x .+. var y
x #+. y = con x .+. y
x @+# y = var x .+. con y
x @+@ y = var x .+. var y
x @+. y = var x .+. y
x .+@ y = x     .+. var y
x .+# y = x     .+. con y
x .+. y = x     <>  y

infixl 6 #+@
infixl 6 #+.
infixl 6 @+#
infixl 6 @+@
infixl 6 @+.
infixl 6 .+#
infixl 6 .+@
infixl 6 .+.

-- $subtraction
--
-- We can summarize the subtraction operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '-'    | '#-@'  | '#-.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@-#'  | '@-@'  | '@-.'            |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.-#'  | '.-@'  | '.-.'            |
-- +-----------------+--------+--------+------------------+

(#-@) :: Num a => a              -> b              -> LinearExpr a b
(#-.) :: Num a => a              -> LinearExpr a b -> LinearExpr a b
(@-#) :: Num a => b              -> a              -> LinearExpr a b
(@-@) :: Num a => b              -> b              -> LinearExpr a b
(@-.) :: Num a => b              -> LinearExpr a b -> LinearExpr a b
(.-#) :: Num a => LinearExpr a b -> a              -> LinearExpr a b
(.-@) :: Num a => LinearExpr a b -> b              -> LinearExpr a b
(.-.) :: Num a => LinearExpr a b -> LinearExpr a b -> LinearExpr a b

x #-@ y = con x .-. var y
x #-. y = con x .-. y
x @-# y = var x .-. con y
x @-@ y = var x .-. var y
x @-. y = var x .-. y
x .-# y = x     .-. con y
x .-@ y = x     .-. var y
x .-. y = x     .+. (-1) #*. y

infixl 6 #-@
infixl 6 #-.
infixl 6 @-#
infixl 6 @-@
infixl 6 @-.
infixl 6 .-#
infixl 6 .-@
infixl 6 .-.

-- $multiplication
--
-- We can summarize the multiplication operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '*'    | '#*@'  | '#*.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@*#'  |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.*#'  |        |                  |
-- +-----------------+--------+--------+------------------+
--
-- As there are few possibilities for valid multiplication, it can be
-- convenient to define e.g. @.*@ or some other short operator as an
-- alias for '#*@'.

(#*.) :: Num a => a              -> LinearExpr a b -> LinearExpr a b
(.*#) :: Num a => LinearExpr a b -> a              -> LinearExpr a b
(#*@) :: Num a => a              -> b              -> LinearExpr a b
(@*#) :: Num a => b              -> a              -> LinearExpr a b

x #*@ y = var y .*# x
x #*. y = y     .*# x
x @*# y = var x .*# y
x .*# y = first (* y) x

infixl 7 #*@
infixl 7 #*.
infixl 7 @*#
infixl 7 .*#

-- $division
--
-- We can summarize the multiplication operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '/'    |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@/#'  |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Expression       | './#'  |        |                  |
-- +-----------------+--------+--------+------------------+
--
-- As there are few possibilities for valid division, it
-- can be convenient to define e.g. @./@ or some other short operator
-- as an alias for '@/#'.

(@/#) :: Fractional a => b -> a -> LinearExpr a b
(./#) :: Fractional a => LinearExpr a b -> a -> LinearExpr a b

x @/# y = var x ./# y
x ./# y = first (/ y) x

infixl 7 @/#
infixl 7 ./#

-- | Combine equivalent terms by summing their coefficients.
simplify :: (Ord b, Num a) => LinearExpr a b -> LinearExpr a b
simplify (LinearExpr terms constant)
  = LinearExpr (reduce (sortOn snd terms)) constant
  where
    reduce []           = []
    reduce ((c, x): []) = [(c, x)]
    reduce ((c, x): (c', x'): xs)
      | x == x'   = (c + c', x) : reduce xs
      | otherwise = (c, x) : reduce ((c', x'): xs)

-- | Reduce an expression to its value.
eval :: Num a => LinearExpr a a -> a
eval (LinearExpr terms constant) = constant + sum (map (uncurry (*)) terms)
