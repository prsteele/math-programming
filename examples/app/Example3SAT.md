# Introduction

This is a literate Haskell file.

We want to solve [3SAT](https://en.wikipedia.org/wiki/3SAT) problems
using the
[`math-programming`](github.com/prsteele/math-programming/math-programming)
and
[`math-programming-glpk`](github.com/prsteele/math-programming/math-programming-glpk)
libraries.

## Notation

We will first describe the 3SAT problem. An instance of 3SAT consists
of zero or more _clauses_, each of which contains precisely three
_terms_. A _term_ consists of a _variable_ and a _sign_. In the 3SAT
instance

$$
    (x \lor y \lor \lnot z) \land (x \lor y \lor z)
$$

there are two clauses, $x \lor y \lor \lnot z$ and $\lor x \lor y \lor z$.
The first clause has the terms $x$, $y$, and $\lnot z$. In this
clause the variables $x$ and $y$ have positive sign, and the variable
$z$ has negative sign. In the second clause all three variables have
positive sign.

A candidate solution to a 3SAT instance is an assignment of all
variables to a sign. A clause is satisfied if at least one of its
terms has a variable that matches its sign; the problem as a whole is
satisfied if all clauses are satisfied. If no such satisfying
assignment exists, we say the problem is unsatisfiable.

## Formulation

Our formulation will map each 3SAT variable to a binary variable.
Suppose we have 3SAT instance

$$
    \bigwedge_{i=1}^n \left( s(i, 1) v(i, 1) \lor s(i, 2) v(i, 2) \lor
    s(i, 3) v(i, 3) \right)
$$

where $v(i, j)$ maps to some variable, $s(i, j) = 1$ indicates that
variable $v(i, j)$ is a positive literal in the term, and $s(i, j) = -1$
indicates that variable $v(i, j)$ is a negative literal in the
term.

To model this instance, we introduce one binary variable for
each 3SAT variable; let $w(i, j)$ denote the binary variable
associated with the 3SAT variable $v(i, j)$ For each clause $i$, we
introduce the constraint

$$
    f(i, 1) + f(i, 2) + f(i, 3) \ge 1
$$

where

$$
    f(i, j) = \begin{cases}
      w(i, j), & s(i, j) > 0 \\
      1 - w(i, j), & s(i, j) < 0 \\
    \end{cases}
$$

We have a zero objective function. If the mixed-integer program is
infeasible, we claim the 3SAT instance is unsatisfiable. Otherwise,
for a feasible solution we assign $v(i, j)$ to true when $w(i, j) = 1$
and $v(i, j)$ to false otherwise.

## Preamble

We add some language extensions.

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
```

We import some common libraries,

```haskell
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
```

as well as ones for modeling and solving mixed-integer programs.

```haskell
import Math.Programming
import Math.Programming.Glpk
```

# Describing a 3SAT instance in Haskell

First, we should model a 3SAT instance abstractly in Haskell. We have
variables,

```haskell
newtype Var = Var T.Text
  deriving (Show, Eq, Ord)
```

terms in a clause,

```haskell
data Term
  = Positive Var
  | Negative Var
  deriving (Show)

```

clauses,

```haskell
type Clause = (Term, Term, Term)
```

and a problem represented by a list of clauses.

```haskell
type Problem = [Clause]
```

The 3SAT instance

$$
    (x \lor y \lor \lnot z) \land (x \lor y \lor z)
$$

would be represented with these types as

```haskell<!-- Example -->
>>> [ (Positive (Var "x"), Positive (Var "y"), Negative (Var "z"))
    , (Positve (Var "x"), Positive (Var "y"), Positive (Var "z"))
    ]
```

# Parsing the problem input

We need to be able to parse some textual representation of a 3SAT
instance. We'll use a format where each line is a clause, each clause
contains three terms separated by whitespace, and a term is a variable
with an optional leading "-" indicating logical negation. In this
format, we would represent

$$
    (x \lor y \lor \lnot z) \land (x \lor y \lor z)
$$

as

```
x y -z
x y z
```

This format is simple enough that we can get by without using
a library like Megaparsec.

```haskell
parseProblem :: T.Text -> Either T.Text Problem
parseProblem input = mapM parseClause (T.lines input)
  where
    parseClause :: T.Text -> Either T.Text (Term, Term, Term)
    parseClause clause = case T.words clause of
      [x, y, z] -> (,,) <$> term x <*> term y <*> term z
      _ -> Left ("failed to parse " <> clause)

    term :: T.Text -> Either T.Text Term
    term var = case T.stripPrefix "-" var of
      Nothing -> Right (Positive (Var var))
      Just v -> if T.null v then Left var else Right (Negative (Var v))
```

# Creating the mixed-integer program

We'll need to keep track of the variables we create. We use the state
monad to do so. Our problem is simple enough to avoid introducing a
new monad type wrapping the state monad, but we do so anyway for clarity.

```haskell
newtype AppM a = AppM {unAppM :: StateT (M.Map Var GlpkVariable) Glpk a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadGlpk,
      MonadIO,
      MonadIP GlpkVariable GlpkConstraint GlpkObjective,
      MonadLP GlpkVariable GlpkConstraint GlpkObjective,
      MonadState (M.Map Var GlpkVariable)
    )

runAppM :: AppM a -> IO a
runAppM = runGlpk . flip evalStateT M.empty . unAppM
```

Note that our base monad for the `StateT` transformer is `Glpk`, which
is itself an instance of `MonadIO`. (Internally, `Glpk` is just a
`ReaderT GlpkEnv IO`.) The list of deriving clauses is unfortunately long.

We could iterate over all clauses to collect the unique set of
variables, and then allocate a binary variable for each; instead,
we'll be lazy, and create new variables as we see them.

```haskell
getVar :: (MonadIP v c o m, MonadState (M.Map Var v) m) => Var -> m v
getVar v@(Var name) = do
  vars <- get
  case M.lookup v vars of
    -- If we already have a variable, return it.
    Just x -> pure x
    -- We haven't seen this variable; make one and return it.
    Nothing -> do
      x <- binary
      setVariableName x name
      put (M.insert v x vars)
      pure x
```

Note that we've annotated `getVar` in a very general form; we could just as easily have written

```haskell<!-- example -->
getVar :: Var -> AppM GlpkVariable
```

We can now define the function $f$ in our formulation as

```haskell
termExpr :: Term -> AppM (Expr GlpkVariable)
termExpr (Positive v) = do
  x <- getVar v
  pure (1 *. x)
termExpr (Negative v) = do
  x <- getVar v
  pure (con 1 .-. var x)
```

Note that we need to keep in mind the difference between our abstract
Haskell variables of type `Var`, the mixed-integer variables of type
`GlpkVariable`, and _expressions_ of `GlpkVariable`s of type `Expr
GlpkVariable`. Note that we used two different approaches to
constructing an `Expr GlpkVariable` from a `GlpkVariable`. The
function `var` has type

```haskell<!-- example -->
var :: Num a => b -> LinExpr a b
```

and simply lifts a variable to an expression containing just that
variable. Writing `var x` is entirely equivalent to writing `1 *. x`.
Similarly, we use `con` to introduce a constant term into a linear
expression.

We can now make a function that adds a constraint, given a clause.

```haskell
addClause :: Clause -> AppM ()
addClause (x, y, z) = do
  vx <- termExpr x
  vy <- termExpr y
  vz <- termExpr z
  vx .+. vy .+. vz .>= 1
  pure ()
```

Note that `.>=` has type

```haskell<!-- example -->
(.>=) :: MonadLP v c o m => Expr v -> Double -> m c
```

and so itself introduces the constraint to the problem! The
generated constraint is also returned by `.>=`, but unless you are
interested in e.g. querying the dual value of the constraint, you are
free to ignore the return value.

We've made variables and constraints, so we can now create the entire program.

```haskell
program :: Problem -> AppM (Maybe (M.Map Var Double))
program clauses = do
  -- Generate all constraints, and as a side effect create all variables
  mapM_ addClause clauses

  -- Solve the problem
  status <- optimizeIP

  case status of
    Error -> liftIO (print "Error") >> pure Nothing
    Infeasible -> pure Nothing
    Unbounded -> pure Nothing

    -- This matches both 'Optimal' and 'Feasible', which are equivalent
    -- in this formulation
    _ -> do
      vars <- get
      varMap <- mapM getVariableValue vars
      pure (Just varMap)
```

When we find a satisfying assignment, we'd like to print it.

```haskell
printAssignment :: M.Map Var Double -> IO ()
printAssignment vars = void (flip M.traverseWithKey vars $ \k v -> printf "%s is %s\n" (show k) (sign_ v))
  where
    sign_ v = show (v >= 0.5)
```

Finally, we'd like to attempt to solve the 3SAT problem provided on stdin.

```haskell
solve :: Problem -> IO ()
solve problem = do
  result <- runAppM (program problem)
  case result of
    Nothing -> putStrLn "Unsatisfiable"
    Just vars -> do
      putStrLn "Satisfiable"
      printAssignment vars

main :: IO ()
main = do
  eProblem <- parseProblem <$> TIO.getContents
  case eProblem of
    Left err -> TIO.putStrLn err
    Right problem -> solve problem
```

## Examples of use

First, we should verify that the set of empty clauses is satisfiable:

```
$ ./result/bin/3sat < /dev/null
Satisfiable
```

So far, so good. What about our trivial example we've been working with,

$$
    (x \lor y \lor \lnot z) \land (x \lor y \lor z)
$$

Note that setting either $x$ or $y$ to true solves both clauses. We find

```
$ echo -e "x y -z\nx y z" | ./result/bin/3sat
Satisfiable
Var "x" is True
Var "y" is False
Var "z" is False
```

as expected. What about an unsatisfiable instance?

```
$ echo -e "x x x\n-x -x -x" | ./result/bin/3sat
Unsatisfiable
```
