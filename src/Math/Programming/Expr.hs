{-| Symbolic linear expressions.

This module defines operators to manipulate linear expressions. Note
that operators have a @.@ on one or both sides, for example '.+' or
'.+.'. An argument abutting a @.@ will always be a 'LinearExpr a
b'. For example, '.+.' adds two linear expressions together, while
'.+' adds a linear expression to a constant.

-}
module Math.Programming.Expr where

import Data.List (sortOn)

-- | A linear expression containing variables of type @b@ and numeric
-- coefficients of type @a@.
--
-- Using strings to denote variables and 'Double's as our numeric
-- type, we could express the term /3 x + 2 y + 1/ as
--
-- @
--   LinearExpr [(3, "x"), (2, "y")] 1
-- @
data LinearExpr a b
  = LinearExpr
    { _terms :: [(a, b)]
    , _constant :: a
    }
  deriving
    ( Read
    , Show
    )

instance (Num a) => Semigroup (LinearExpr a b) where
  (<>) = (.+.)

instance (Num a) => Monoid (LinearExpr a b) where
  mempty = LinearExpr [] 0

instance Functor (LinearExpr a) where
  fmap f (LinearExpr terms constant) = LinearExpr (fmap (fmap f) terms) constant

sumExpr :: (Num a) => [LinearExpr a b] -> LinearExpr a b
sumExpr = mconcat

(*:) :: (Num a) => a -> b -> LinearExpr a b
coef *: term = LinearExpr [(coef, term)] 0

(.+.) :: (Num a) => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
(LinearExpr terms constant) .+. (LinearExpr terms' constant')
  = LinearExpr (terms <> terms') (constant + constant')
infixl 6 .+.

(.+) :: (Num a) => LinearExpr a b -> a -> LinearExpr a b
ex .+ constant = ex .+. LinearExpr [] constant
infixl 6 .+

(+.) :: (Num a) => a -> LinearExpr a b -> LinearExpr a b
(+.) = flip (.+)
infixl 6 +.

(.-.) :: (Num a) => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
ex .-. ex' = ex .+. (ex' .* (-1))
infixl 6 .-.

(.-) :: (Num a) => LinearExpr a b -> a -> LinearExpr a b
ex .- constant = ex .+ (negate constant)
infixl 6 .-

(-.) :: (Num a) => a -> LinearExpr a b -> LinearExpr a b
constant -. ex = constant +. ((-1) *. ex)
infixl 6 -.

(.*) :: (Num a) => LinearExpr a b -> a -> LinearExpr a b
LinearExpr terms constant .* coef
  = LinearExpr (fmap scale terms) (constant * coef)
  where
    scale (x, y) = (x * coef, y)
infixl 7 .*

(*.) :: (Num a) => a -> LinearExpr a b -> LinearExpr a b
(*.) = flip (.*)
infixl 7 *.

(./) :: (Num a, Fractional a) => LinearExpr a b -> a -> LinearExpr a b
ex ./ coef = ex .* (1 / coef)
infixl 7 ./

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
