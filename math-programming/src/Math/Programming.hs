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

    -- ** Creating constraints
    -- $constraints

    -- *** Equality constraints
    (.==.),
    (==.),
    (.==),

    -- *** Less-than constraints
    (.<=.),
    (<=.),
    (.<=),

    -- *** Greater-than constraints
    (.>=.),
    (>=.),
    (.>=),

    -- ** Creating objectives
    minimize,
    maximize,

    -- ** Creating linear expressions
    -- $expressions
    var,
    con,
    (.*),
    (*.),
    (.+.),
    (.-.),
    (./),
    eval,
    simplify,
    vsum,
    esum,
    scale,

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
    Inequality (..),
    LinExpr (..),

    -- * Naming model attributes
    withVariableName,
    withConstraintName,
    withObjectiveName,
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
-- A @'LinExpr' a b@ is a linear expression over variables of type @b@
-- with coefficients of type @a@ (typically 'Double'.) We provide a
-- number of operators to build up linear expressions naturally. The
-- mnemonic is that @.@ characters point to expressions.

-- $constraints
--
-- An @'Inequality' a@ is an inequality over the type @a@, which in
-- turn is typically an @'Expr' v@ for some variable type @v@. Despite
-- the name, 'Inequality' can also represent equality constraints
-- directly.
--
-- As an alternative to constructing an inequality and passing it to
-- 'addConstraint', we can use the convenience operators below. Since
-- linear programming constraints often involve constant bounds, we
-- offer operators specialized for both expressions and constants. The
-- mnemonic is that @.@ characters point to expressions

-- $mathprograms
--
-- The 'MonadLP' and 'MonadIP' classes provide low-level APIs for
-- defining linear and integer programs, respectively, although the
-- high-level DSL will typically be easier to work with.
