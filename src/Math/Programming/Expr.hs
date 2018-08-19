{-| Symbolic linear expressions.

This module defines operators to manipulate linear expressions. Note
that operators have a @.@ on one or both sides, for example '.+' or
'.+.'. An argument abutting a @.@ will always be a 'LinearExpr a
b'. For example, '.+.' adds two linear expressions together, while
'.+' adds a linear expression to a constant.

-}
module Math.Programming.Expr where

import Data.List (sortOn)

-- | A linear expression containing variables of type @a@ and numeric
-- coefficients of type @b@.
--
-- Using strings to denote variables and 'Double's as our numeric
-- type, we could express the term /3 x + 2 y + 1/ as
--
-- @
--   LinearExpr [("x", 3), ("y", 2")] 1
-- @
data LinearExpr a b
  = LinearExpr
    { _terms :: [(a, b)]
    , _constant :: b
    }
  deriving
    ( Read
    , Show
    )

instance (Num b) => Semigroup (LinearExpr a b) where
  (<>) = (.+.)

instance (Num b) => Monoid (LinearExpr a b) where
  mempty = LinearExpr [] 0

(*:) :: (Num b) => b -> a -> LinearExpr a b
constant *: term = LinearExpr [(term, constant)] 0

(.+.) :: (Num b) => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
(LinearExpr terms constant) .+. (LinearExpr terms' constant')
  = LinearExpr (terms <> terms') (constant + constant')
infixl 6 .+.

(.+) :: (Num b) => LinearExpr a b -> b -> LinearExpr a b
ex .+ constant = ex .+. LinearExpr [] constant
infixl 6 .+

(+.) :: (Num b) => b -> LinearExpr a b -> LinearExpr a b
(+.) = flip (.+)
infixl 6 +.

(.-.) :: (Num b) => LinearExpr a b -> LinearExpr a b -> LinearExpr a b
ex .-. ex' = ex .+. (ex' .* (-1))
infixl 6 .-.

(.-) :: (Num b) => LinearExpr a b -> b -> LinearExpr a b
ex .- constant = ex .+ (negate constant)
infixl 6 .-

(-.) :: (Num b) => b -> LinearExpr a b -> LinearExpr a b
constant -. ex = constant +. ((-1) *. ex)
infixl 6 -.

(.*) :: (Num b) => LinearExpr a b -> b -> LinearExpr a b
LinearExpr terms constant .* coef
  = LinearExpr (fmap scale terms) (constant * coef)
  where
    scale (x, y) = (x, y * coef)
infixl 7 .*

(*.) :: (Num b) => b -> LinearExpr a b -> LinearExpr a b
(*.) = flip (.*)
infixl 7 *.

(./) :: (Num b, Fractional b) => LinearExpr a b -> b -> LinearExpr a b
ex ./ coef = ex .* (1 / coef)
infixl 7 ./

-- | Combine equivalent terms by summing their coefficients
simplify :: (Ord a, Num b) => LinearExpr a b -> LinearExpr a b
simplify (LinearExpr terms constant)
  = LinearExpr (reduce (sortOn fst terms)) constant
  where
    reduce []           = []
    reduce ((x, c): []) = [(x, c)]
    reduce ((x, c): (x', c'): xs)
      | x == x'   = (x, c + c') : reduce xs
      | otherwise = (x, c) : reduce xs
