-- | A <https://www.gnu.org/software/glpk/ GLPK> backend to the 'Math.Programming' library.
--
-- This package allows both linear and mixed-integer programs to be solved.
module Math.Programming.Glpk
  ( Glpk,
    GlpkVariable,
    GlpkConstraint,
    GlpkObjective,
    runGlpk,
    writeFormulation,
    GlpkT,
    MonadGlpk,

    -- ** Controlling GLPK behavior
    -- $settings
    GlpkEnv,
    GlpkException,
  )
where

import Math.Programming.Glpk.Internal

-- $settings
--
-- See the 'Math.Programming.Glpk.Header' package for Haskell wrappers
-- for all low-level GLPK operations.
