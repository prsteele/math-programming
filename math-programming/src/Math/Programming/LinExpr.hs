{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections #-}

-- | Linear expressions of variables.
module Math.Programming.LinExpr where

import Data.List (foldl', sortOn)

-- | A linear expression.
--
-- Linear expressions contain symbolic variables of type @b@ and
-- numeric coefficients of type @a@. Often @a@ will be 'Double', and
-- @b@ will be whatever variable type your linear program uses.
data LinExpr a b
  = LinExpr ![(a, b)] !a
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)

instance Num a => Semigroup (LinExpr a b) where
  (<>) = (.+)

instance Num a => Monoid (LinExpr a b) where
  mempty = con 0

-- | Construct a term in a linear expression by multiplying a constant
-- by a variable.
(*.) :: Num a => a -> b -> LinExpr a b
(*.) x y = LinExpr [(x, y)] 0

infixl 7 .*

-- | Construct a term in a linear expression by multiplying a variable
-- by a constant.
(.*) :: Num a => b -> a -> LinExpr a b
(.*) = flip (*.)

infixl 7 *.

-- | Construct a term in a linear expression by dividing a variable by
-- a constant.
(./) :: Fractional a => b -> a -> LinExpr a b
(./) x y = x .* (1 / y)

infixl 7 ./

-- | Multiplication of linear expressions by a constant.
scale :: Num a => a -> LinExpr a b -> LinExpr a b
scale coef (LinExpr terms constant) = LinExpr terms' constant'
  where
    terms' = [(c * coef, x) | (c, x) <- terms]
    constant' = constant * coef

-- | Addition of linear expressions.
(.+) :: Num a => LinExpr a b -> LinExpr a b -> LinExpr a b
(.+) (LinExpr terms constant) (LinExpr terms' constant') =
  LinExpr (terms <> terms') (constant + constant')

infixl 6 .+

-- | The difference of linear expressions.
(.-) :: Num a => LinExpr a b -> LinExpr a b -> LinExpr a b
(.-) x y = x .+ scale (-1) y

infixl 6 .-

-- | A linear expression with a single variable term.
var :: Num a => b -> LinExpr a b
var x = LinExpr [(1, x)] 0

-- | A linear expression with only a constant term.
con :: a -> LinExpr a b
con = LinExpr []

-- | The sum of variable terms with coefficients of unity.
vsum :: Num a => [b] -> LinExpr a b
vsum = flip LinExpr 0 . fmap (1,)

-- | The sum of linear expressions.
esum :: Num a => Foldable t => t (LinExpr a b) -> LinExpr a b
esum = foldl' (.+) mempty

-- | Reduce an expression to its value.
eval :: Num a => LinExpr a a -> a
eval (LinExpr terms constant) = constant + sum' (map (uncurry (*)) terms)
  where
    sum' = foldl' (+) 0

-- | Simplify an expression by grouping like terms.
simplify :: (Num a, Ord b) => LinExpr a b -> LinExpr a b
simplify (LinExpr terms constant) =
  LinExpr (foldr f [] (sortOn snd terms)) constant
  where
    f (c, x) [] = [(c, x)]
    f (c, x) ((c', x') : xs) =
      if x == x'
        then (c + c', x) : xs
        else (c, x) : (c', x') : xs
