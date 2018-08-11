module Math.Programming.Expr where

data LinearExpr a
  = Constant Double
  | Term a
  | SumExpr (LinearExpr a) (LinearExpr a)
  | ProductExpr (LinearExpr a) Double
  deriving
    ( Show
    )

(.+.) :: LinearExpr a -> LinearExpr a -> LinearExpr a
(.+.) = SumExpr
infixl 6 .+.

(.+) :: LinearExpr a -> Double -> LinearExpr a
ex .+ constant = ex .+. Constant constant
infixl 6 .+

(+.) :: Double -> LinearExpr a -> LinearExpr a
(+.) = flip (.+)
infixl 6 +.

(.-.) :: LinearExpr a -> LinearExpr a -> LinearExpr a
ex .-. ex' = ex .+. (ex' .* (-1))
infixl 6 .-.

(.-) :: LinearExpr a -> Double -> LinearExpr a
ex .- constant = ex .-. Constant constant
infixl 6 .-

(-.) :: Double -> LinearExpr a -> LinearExpr a
constant -. ex = Constant constant .-. ex
infixl 6 -.

(.*) :: LinearExpr a -> Double -> LinearExpr a
(.*) = ProductExpr
infixl 7 .*

(*.) :: Double -> LinearExpr a -> LinearExpr a
(*.) = flip (.*)
infixl 7 *.

(./) :: LinearExpr a -> Double -> LinearExpr a
ex ./ coef = ex .* (1 / coef)
infixl 7 ./

flattenExpr :: LinearExpr a -> ([(a, Double)], Double)
flattenExpr (Constant constant) = ([], constant)
flattenExpr (Term term) = ([(term, 1.0)], 0)
flattenExpr (SumExpr ex ex') = (terms ++ terms', constant + constant')
  where
    (terms, constant) = flattenExpr ex
    (terms', constant') = flattenExpr ex'
flattenExpr (ProductExpr ex scaling) = (fmap scale terms, constant * scaling)
  where
    (terms, constant) = flattenExpr ex
    scale (term, coef) = (term, coef * scaling)
