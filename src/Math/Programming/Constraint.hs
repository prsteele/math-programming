module Math.Programming.Constraint where

import Math.Programming.Expr

newtype ConstraintId
  = ConstraintId { fromConstraintId :: Int }
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    )

data Constraint a b
  = Constraint (LinearExpr a b) Ordering
  deriving
    ( Read
    , Show
    )

(.<=.) :: (Num b) => LinearExpr a b -> LinearExpr a b -> Constraint a b
lhs .<=. rhs = Constraint (lhs .-. rhs) LT
infix 4 .<=.

(.<=) :: (Num b) => LinearExpr a b -> b -> Constraint a b
lhs .<= rhs = lhs .<=. LinearExpr [] rhs
infix 4 .<=

(<=.) :: (Num b) => b -> LinearExpr a b -> Constraint a b
lhs <=. rhs = LinearExpr [] lhs .<=. rhs
infix 4 <=.

(.>=.) :: (Num b) => LinearExpr a b -> LinearExpr a b -> Constraint a b
lhs .>=. rhs = Constraint (lhs .-. rhs) GT
infix 4 .>=.

(.>=) :: (Num b) => LinearExpr a b -> b -> Constraint a b
lhs .>= rhs = lhs .>=. LinearExpr [] rhs
infix 4 .>=

(>=.) :: (Num b) => b -> LinearExpr a b -> Constraint a b
lhs >=. rhs = LinearExpr [] lhs .>=. rhs
infix 4 >=.

(.==.) :: (Num b) => LinearExpr a b -> LinearExpr a b -> Constraint a b
lhs .==. rhs = Constraint (lhs .-. rhs) EQ
infix 4 .==.

(.==) :: (Num b) => LinearExpr a b -> b -> Constraint a b
lhs .== rhs = lhs .==. LinearExpr [] rhs
infix 4 .==

(==.) :: (Num b) => b -> LinearExpr a b -> Constraint a b
lhs ==. rhs = LinearExpr [] lhs .==. rhs
infix 4 ==.
