{-| Non-strict linear inequalities over symbolic linear expressions.

We follow the same conventions as in 'Math.Programming.Expression'
where @#@ abut constants, @\@@ abut variables, and @.@ abut
expressions.
-}
module Math.Programming.Constraint
  ( -- * Construction
    Inequality (..)
    -- ** Less-than constraints
    -- $lt
  , (#<=@)
  , (#<=.)
  , (@<=#)
  , (@<=@)
  , (@<=.)
  , (.<=#)
  , (.<=@)
  , (.<=.)
    -- ** Greater-than constraints
    -- $gt
  , (#>=@)
  , (#>=.)
  , (@>=#)
  , (@>=@)
  , (@>=.)
  , (.>=#)
  , (.>=@)
  , (.>=.)
  -- ** Equality constraints
  -- $eq
  , (#==@)
  , (#==.)
  , (@==#)
  , (@==@)
  , (@==.)
  , (.==#)
  , (.==@)
  , (.==.)
  ) where

import           Data.Traversable            (fmapDefault, foldMapDefault)

import           Math.Programming.Expression

-- | Non-strict inequalities.
data Inequality a
  = Inequality Ordering a a
  deriving
    ( Read
    , Show
    )

instance Functor Inequality where
  fmap = fmapDefault

instance Foldable Inequality where
  foldMap = foldMapDefault

instance Traversable Inequality where
  traverse f (Inequality sense lhs rhs)
    = Inequality sense <$> f lhs <*> f rhs

(#<=@) :: Num a => a              -> b              -> Inequality (LinearExpr a b)
(#<=.) :: Num a => a              -> LinearExpr a b -> Inequality (LinearExpr a b)
(@<=#) :: Num a => b              -> a              -> Inequality (LinearExpr a b)
(@<=@) :: Num a => b              -> b              -> Inequality (LinearExpr a b)
(@<=.) :: Num a => b              -> LinearExpr a b -> Inequality (LinearExpr a b)
(.<=#) :: Num a => LinearExpr a b -> a              -> Inequality (LinearExpr a b)
(.<=@) :: Num a => LinearExpr a b -> b              -> Inequality (LinearExpr a b)
(.<=.) :: Num a => LinearExpr a b -> LinearExpr a b -> Inequality (LinearExpr a b)
(#>=@) :: Num a => a              -> b              -> Inequality (LinearExpr a b)
(#>=.) :: Num a => a              -> LinearExpr a b -> Inequality (LinearExpr a b)
(@>=#) :: Num a => b              -> a              -> Inequality (LinearExpr a b)
(@>=@) :: Num a => b              -> b              -> Inequality (LinearExpr a b)
(@>=.) :: Num a => b              -> LinearExpr a b -> Inequality (LinearExpr a b)
(.>=#) :: Num a => LinearExpr a b -> a              -> Inequality (LinearExpr a b)
(.>=@) :: Num a => LinearExpr a b -> b              -> Inequality (LinearExpr a b)
(.>=.) :: Num a => LinearExpr a b -> LinearExpr a b -> Inequality (LinearExpr a b)
(#==@) :: Num a => a              -> b              -> Inequality (LinearExpr a b)
(#==.) :: Num a => a              -> LinearExpr a b -> Inequality (LinearExpr a b)
(@==#) :: Num a => b              -> a              -> Inequality (LinearExpr a b)
(@==@) :: Num a => b              -> b              -> Inequality (LinearExpr a b)
(@==.) :: Num a => b              -> LinearExpr a b -> Inequality (LinearExpr a b)
(.==#) :: Num a => LinearExpr a b -> a              -> Inequality (LinearExpr a b)
(.==@) :: Num a => LinearExpr a b -> b              -> Inequality (LinearExpr a b)
(.==.) :: Num a => LinearExpr a b -> LinearExpr a b -> Inequality (LinearExpr a b)

x #<=@ y = Inequality LT (con x) (var y)
x #<=. y = Inequality LT (con x) y
x @<=# y = Inequality LT (var x) (con y)
x @<=@ y = Inequality LT (var x) (var y)
x @<=. y = Inequality LT (var x) y
x .<=# y = Inequality LT x       (con y)
x .<=@ y = Inequality LT x       (var y)
x .<=. y = Inequality LT x       y
x #>=@ y = Inequality GT (con x) (var y)
x #>=. y = Inequality GT (con x) y
x @>=# y = Inequality GT (var x) (con y)
x @>=@ y = Inequality GT (var x) (var y)
x @>=. y = Inequality GT (var x) y
x .>=# y = Inequality GT x       (con y)
x .>=@ y = Inequality GT x       (var y)
x .>=. y = Inequality GT x       y
x #==@ y = Inequality EQ (con x) (var y)
x #==. y = Inequality EQ (con x) y
x @==# y = Inequality EQ (var x) (con y)
x @==@ y = Inequality EQ (var x) (var y)
x @==. y = Inequality EQ (var x) y
x .==# y = Inequality EQ x       (con y)
x .==@ y = Inequality EQ x       (var y)
x .==. y = Inequality EQ x       y

infix 4 #<=@
infix 4 #<=.
infix 4 @<=#
infix 4 @<=@
infix 4 @<=.
infix 4 .<=#
infix 4 .<=@
infix 4 .<=.
infix 4 #>=@
infix 4 #>=.
infix 4 @>=#
infix 4 @>=@
infix 4 @>=.
infix 4 .>=#
infix 4 .>=@
infix 4 .>=.
infix 4 #==@
infix 4 #==.
infix 4 @==#
infix 4 @==@
infix 4 @==.
infix 4 .==#
infix 4 .==@
infix 4 .==.

-- $lt
--
-- We can summarize the various inquality operators in the following table.
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         |        | '#<=@' | '#<=.'           |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@<=#' | '@<=@' | '@<=.'           |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.<=#' | '.<=@' | '.<=.'           |
-- +-----------------+--------+--------+------------------+

-- $gt
--
-- We can summarize the various inquality operators in the following table.
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         |        | '#>=@' | '#>=.'           |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@>=#' | '@>=@' | '@>=.'           |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.>=#' | '.>=@' | '.>=.'           |
-- +-----------------+--------+--------+------------------+

-- $eq
--
-- We can summarize the various inquality operators in the following table.
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         |        | '#==@' | '#==.'           |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@==#' | '@==@' | '@==.'           |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.==#' | '.==@' | '.==.'           |
-- +-----------------+--------+--------+------------------+
