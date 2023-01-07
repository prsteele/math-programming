-- | A library for modeling and solving linear and integer programs.
--
-- This library is merely a frontend to various solver backends. At
-- the time this was written, the only known supported backend is
-- <https://github.com/prsteele/math-programming/blob/main/math-programming-glpk/README.md GLPK>.
--
-- This page includes a high-level overview of the model building DSL,
-- as well as a deeper dive into the core monadic interface.
module Math.Programming
  ( -- * Model-building DSL
    -- $models

    -- ** Creating variables

    -- *** Continuous variables
    free,
    bounded,
    nonNeg,
    nonPos,

    -- *** Discrete variables
    integer,
    binary,
    nonNegInteger,
    nonPosInteger,

    -- *** Modifying variables
    -- $modifiers
    within,
    asKind,
    named,

    -- ** Creating constraints
    -- $constraints

    -- *** Equality constraints
    (.==.),
    (#==.),
    (.==#),

    -- *** Less-than constraints
    (.<=.),
    (#<=.),
    (.<=#),

    -- *** Greater-than constraints
    (.>=.),
    (#>=.),
    (.>=#),

    -- ** Creating objectives
    minimize,
    maximize,

    -- ** Creating linear expressions
    -- $expressions
    (.*),
    (.+),
    (.-),
    (./),
    eval,
    simplify,
    var,
    con,
    vsum,
    esum,

    -- ** Naming model components
    -- $naming

    -- * Math programs
    -- $mathprograms

    -- ** Linear programs
    MonadLP (..),

    -- ** Integer programs
    MonadIP (..),
    Domain (..),

    -- * Other types and functions
    evalExpr,
    formatExpr,
    Expr,
    Bounds (..),
    SolutionStatus (..),
    Sense (..),
    Named (..),
    Inequality (..),
    LinExpr (..),
  )
where

import Math.Programming.Dsl
import Math.Programming.LinExpr
import Math.Programming.Types

-- $introduction
--
-- This library provides a monadic interface for building and solving
-- linear and integer programs.

-- $models
--
-- We provide a monadic DSL for specifying math programs. This DSL
-- builds up programs statefully, rather than building some pure,
-- abstract representation of a math program.

-- $modifiers
-- Regardless of the helper functions used above to create a
-- variable, you can modify its behavior using the following
-- modifiers.

-- $expressions
--
-- Since both constraints and objectives involve linear expressions of
-- variables, we need to be able to manipulate them.
--
-- The core of manipulating linear expressions are the '.*' operator,
-- which creates a term in an expression, and '.+', which combines
-- terms into a single expression.

-- $constraints
--
-- We provide a number of operators to create constraints from
-- different types of equality and inequality constraints. There are
-- three variants of each constraint type, based on whether the
-- left-hand side, right-hand side, or both sides of the constraint
-- are expressions. The mnemonic is that @.@ characters point to
-- expressions, while @#@ characters point to constants.

-- $naming
--
-- The 'Named' class allows us to get and set the names of model
-- variables, constraints, and objectives. The 'named' combinator
-- makes it easy to name these as they are created.

-- $mathprograms
--
-- The 'MonadLP' and 'MonadIP' classes provide low-level APIs for
-- defining linear and integer programs, respectively, although the
-- high-level DSL will typically be easier to work with.
