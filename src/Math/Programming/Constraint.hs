module Math.Programming.Constraint where

import Math.Programming.Expr

data Constraint a
  = Constraint (LinearExpr a) Ordering

(.<=.) :: LinearExpr a -> LinearExpr a -> Constraint a
lhs .<=. rhs = Constraint (lhs .-. rhs) LT
infix 4 .<=.

(.<=) :: LinearExpr a -> Double -> Constraint a
lhs .<= rhs = lhs .<=. Constant rhs
infix 4 .<=

(<=.) :: Double -> LinearExpr a -> Constraint a
lhs <=. rhs = Constant lhs .<=. rhs
infix 4 <=.

(.>=.) :: LinearExpr a -> LinearExpr a -> Constraint a
lhs .>=. rhs = Constraint (lhs .-. rhs) GT
infix 4 .>=.

(.>=) :: LinearExpr a -> Double -> Constraint a
lhs .>= rhs = lhs .>=. Constant rhs
infix 4 .>=

(>=.) :: Double -> LinearExpr a -> Constraint a
lhs >=. rhs = Constant lhs .>=. rhs
infix 4 >=.

(.==.) :: LinearExpr a -> LinearExpr a -> Constraint a
lhs .==. rhs = Constraint (lhs .-. rhs) EQ
infix 4 .==.

(.==) :: LinearExpr a -> Double -> Constraint a
lhs .== rhs = lhs .==. Constant rhs
infix 4 .==

(==.) :: Double -> LinearExpr a -> Constraint a
lhs ==. rhs = Constant lhs .==. rhs
infix 4 ==.
