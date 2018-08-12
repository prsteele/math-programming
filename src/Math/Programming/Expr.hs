module Math.Programming.Expr where

data LinearExpr a = LinearExpr [(a, Double)] Double
  deriving
    ( Show
    )

(.+.) :: LinearExpr a -> LinearExpr a -> LinearExpr a
(LinearExpr terms constant) .+. (LinearExpr terms' constant')
  = LinearExpr (terms <> terms') (constant + constant')
infixl 6 .+.

(.+) :: LinearExpr a -> Double -> LinearExpr a
ex .+ constant = ex .+. LinearExpr [] constant
infixl 6 .+

(+.) :: Double -> LinearExpr a -> LinearExpr a
(+.) = flip (.+)
infixl 6 +.

(.-.) :: LinearExpr a -> LinearExpr a -> LinearExpr a
ex .-. ex' = ex .+. (ex' .* (-1))
infixl 6 .-.

(.-) :: LinearExpr a -> Double -> LinearExpr a
ex .- constant = ex .+ (negate constant)
infixl 6 .-

(-.) :: Double -> LinearExpr a -> LinearExpr a
constant -. ex = constant +. ((-1) *. ex)
infixl 6 -.

(.*) :: LinearExpr a -> Double -> LinearExpr a
LinearExpr terms constant .* coef
  = LinearExpr (fmap scale terms) (constant * coef)
  where
    scale (x, y) = (x, y * coef)
infixl 7 .*

(*.) :: Double -> LinearExpr a -> LinearExpr a
(*.) = flip (.*)
infixl 7 *.

(./) :: LinearExpr a -> Double -> LinearExpr a
ex ./ coef = ex .* (1 / coef)
infixl 7 ./
