module Math.Programming.Expr where

data LinearExpr a b = LinearExpr [(a, b)] b
  deriving
    ( Read
    , Show
    )

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
