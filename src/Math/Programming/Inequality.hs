module Math.Programming.Inequality where

import Math.Programming.Expr

-- | An expression of the form a * x + b {>=, ==, <=} 0.
data Inequality a b
  = Inequality (LinearExpr a b) Ordering
  deriving
    ( Read
    , Show
    )

(.<=.) :: (Num a) => LinearExpr a b -> LinearExpr a b -> Inequality a b
lhs .<=. rhs = Inequality (lhs .-. rhs) LT
infix 4 .<=.

(.<=) :: (Num a) => LinearExpr a b -> a -> Inequality a b
lhs .<= rhs = lhs .<=. LinearExpr [] rhs
infix 4 .<=

(<=.) :: (Num a) => a -> LinearExpr a b -> Inequality a b
lhs <=. rhs = LinearExpr [] lhs .<=. rhs
infix 4 <=.

(.>=.) :: (Num a) => LinearExpr a b -> LinearExpr a b -> Inequality a b
lhs .>=. rhs = Inequality (lhs .-. rhs) GT
infix 4 .>=.

(.>=) :: (Num a) => LinearExpr a b -> a -> Inequality a b
lhs .>= rhs = lhs .>=. LinearExpr [] rhs
infix 4 .>=

(>=.) :: (Num a) => a -> LinearExpr a b -> Inequality a b
lhs >=. rhs = LinearExpr [] lhs .>=. rhs
infix 4 >=.

(.==.) :: (Num a) => LinearExpr a b -> LinearExpr a b -> Inequality a b
lhs .==. rhs = Inequality (lhs .-. rhs) EQ
infix 4 .==.

(.==) :: (Num a) => LinearExpr a b -> a -> Inequality a b
lhs .== rhs = lhs .==. LinearExpr [] rhs
infix 4 .==

(==.) :: (Num a) => a -> LinearExpr a b -> Inequality a b
lhs ==. rhs = LinearExpr [] lhs .==. rhs
infix 4 ==.
