-- | A library for modeling and solving linear and integer programs.
--
-- This library is merely a frontend to various solver backends. At the
-- time this was written, the only known supported backend is
-- <https://github.com/prsteele/math-programming-glpk GLPK>.
module Math.Programming
  ( -- * Math programs
    -- $mathprograms

    -- ** Linear programs
    LPMonad (..),
    Expr,
    Bounds (..),
    SolutionStatus (..),
    Sense (..),
    Named (..),

    -- ** Integer programs
    IPMonad (..),
    Domain (..),

    -- * Model-building DSL
    -- $models

    -- ** Creating variables
    -- $variables

    -- *** Continuous variables
    free,
    nonNeg,
    nonPos,
    bounded,
    within,

    -- *** Integer variables
    integer,
    binary,
    nonNegInteger,
    nonPosInteger,
    asKind,

    -- ** Linear expressions
    -- $expressions
    LinExpr (..),
    eval,
    simplify,
    var,
    con,
    vsum,
    esum,
    (.*),
    (.+),
    (.-),
    (./),

    -- ** Constraints
    -- $constraints
    Inequality (..),

    -- *** Less-than constraints
    -- $lt
    (#<=@),
    (#<=.),
    (@<=#),
    (@<=@),
    (@<=.),
    (.<=#),
    (.<=@),
    (.<=.),

    -- *** Greater-than constraints
    -- $gt
    (#>=@),
    (#>=.),
    (@>=#),
    (@>=@),
    (@>=.),
    (.>=#),
    (.>=@),
    (.>=.),

    -- *** Equality constraints
    -- $eq
    (#==@),
    (#==.),
    (@==#),
    (@==@),
    (@==.),
    (.==#),
    (.==@),
    (.==.),

    -- ** Specifying objectives
    minimize,
    maximize,

    -- ** Utilities
    evalExpr,
    named,
  )
where

import Math.Programming.Dsl
import Math.Programming.LinExpr
import Math.Programming.Types

-- $mathprograms
--
-- The 'LPMonad' provides all the primitives necessary to formulate
-- and solve linear programs; the 'IPMonad' provides the same for
-- integer programs. However, you should not often need to use these
-- APIs directly, as we provide more user-friendly functions wrapping
-- these low-level functions below.

-- $models
--
-- The functions in the 'LPMonad' and 'IPMonad' typeclasses are
-- designed to interface with low-level solver backends. We provide a
-- cleaner interface in the following sections.

-- $variables
--
-- 'LPMonad' provides 'addVariable' and 'setVariableBounds', and
-- 'IPMonad' additionally provides 'setVariableDomain'. While
-- sufficient to create your programs, you are encouraged to use the
-- more natural functions below.

-- $expressions
--
-- The module 'Math.Programming.LinExpr' provides operators
-- to build up 'LinExpr' objects using declared variables.

-- $addition
--
-- We can summarize the addition operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '+'    | '#+@'  | '#+.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@+#'  | '@+@'  | '@+.'            |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.+#'  | '.+@'  | '.+.'            |
-- +-----------------+--------+--------+------------------+

-- $subtraction
--
-- We can summarize the subtraction operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '-'    | '#-@'  | '#-.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@-#'  | '@-@'  | '@-.'            |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.-#'  | '.-@'  | '.-.'            |
-- +-----------------+--------+--------+------------------+

-- $multiplication
--
-- We can summarize the multiplication operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '*'    | '#*@'  | '#*.'            |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@*#'  |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Expression       | '.*#'  |        |                  |
-- +-----------------+--------+--------+------------------+
--
-- As there are few possibilities for valid multiplication, it can be
-- convenient to define e.g. @.*@ or some other short operator as an
-- alias for '#*@'.

-- $division
--
-- We can summarize the multiplication operators with the table
--
-- +-----------------+--------+--------+------------------+
-- |                 |Constant|Variable|    Expression    |
-- +-----------------+--------+--------+------------------+
-- |Constant         | '/'    |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Variable         | '@/#'  |        |                  |
-- +-----------------+--------+--------+------------------+
-- |Expression       | './#'  |        |                  |
-- +-----------------+--------+--------+------------------+
--
-- As there are few possibilities for valid division, it
-- can be convenient to define e.g. @./@ or some other short operator
-- as an alias for '@/#'.

-- $constraints
--
-- The 'LPMonad' provides the 'addConstraint' function. However, you
-- will typically use the operators below to directly apply
-- constraints to the model. We follow the same conventions as with
-- our arithmetic operators.

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
