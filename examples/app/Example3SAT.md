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
    (x \lor y \lor \lnot z) \land (\lor x \lor y \lor z)
$$

there are two clauses, $x \lor y \lor \lnot z$ and $\lor x \lor y \lor
z$. The first clause has the terms $x$, $y$, and $\lnot z$. In this
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
variable $v(i, j)$ is a positive literal in the term, and $s(i, j) =
-1$ indicates that variable $v(i, j)$ is a negative literal in the
term. Let $w(i, j)$ denote the binary MIP variable associated with the
3SAT variable $v(i, j)$ To model this instance, we introduce one
binary variable for each 3SAT variable. For each clause $i$, we
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
for a feasible solution $\hat y{i, j}$

## Preamble

We need some language extensions, for convenience.

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

# Describing a 3SAT instance

First, we should model a 3SAT instance abstractly in Haskell. We have
variables,

```haskell
newtype Var = Var T.Text
  deriving (Show, Eq, Ord)
```

terms in a clause,

```haskell
-- | A term in a 3SAT clause.
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
    (x \lor y \lor \lnot z) \land (\lor x \lor y \lor z)
$$

would be represented with these types as

```haskell<!-- Example -->
>>> [ (Positive (Var "x"), Positive (Var "y"), Negative (Var "z"))
    , (Negative (Var "x"), Positive (Var "y"), Negative (Var "z"))
    ]
```

# Parsing problem input

We need to be able to parse some textual representation of a 3SAT
instance. We'll use a format where each line is a clause, each clause
contains three terms separated by whitespace, and a term is a variable
with an optional leading "-" indicating logical negation. In this
format, we would represent

$$
    (x \lor y \lor \lnot z) \land (\lor x \lor y \lor z)
$$

as

```
x y -z
-x y z
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
      _ -> Left clause

    term :: T.Text -> Either T.Text Term
    term var = case T.stripPrefix "-" var of
      Nothing -> Right (Positive (Var var))
      Just v -> if T.null v then Left var else Right (Negative (Var v))
```

# Creating the mixed-integer program

Once we've parsed our input to create a number of clauses, we need to
formulate our mixed-integer program. Our formulation will map each
3SAT variable to a binary variable. Each clause will map to a
constraint, and we will have a zero objective function. If we have the
clause $s_1 x_1 \lor s_2 x_2 \lor s_3 x_3$, where $s_i$ is the sign of
variable $x_i$, then we introduce the constraint

$$
    f(s_1, x_1) + f(s_2, x_2) + f(s_3, x_3) \ge 1
$$

where

$$
    f(x, x) = \begin{cases}
      x, & s \text{is a positive} \\
      1 - x, & s \text{is negative} \\
    \end{cases}
$$




```
-- | Our program state. We track the variables introduced in the State Monad.
--
-- If necessary, we could also track the constraints and objective function.
newtype AppM a = AppM {unAppM :: StateT (M.Map Var GlpkVariable) Glpk a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState (M.Map Var GlpkVariable),
      Named GlpkVariable,
      Named GlpkConstraint,
      Named GlpkObjective,
      MonadLP GlpkVariable GlpkConstraint GlpkObjective,
      MonadIP GlpkVariable GlpkConstraint GlpkObjective
    )

runAppM :: AppM a -> IO a
runAppM = runGlpk . flip evalStateT M.empty . unAppM

-- | Get or create the MIP variable associated with a 'Var'.
--
-- If the 'Var' has not yet been added to the problem, we create one.
getVar :: (MonadIP v c o m, MonadState (M.Map Var v) m) => Var -> m v
getVar v@(Var name) = do
  vars <- get
  case M.lookup v vars of
    Just x -> pure x
    Nothing -> do
      x <- binary `named` name
      put (M.insert v x vars)
      pure x

addClause :: Clause -> AppM ()
addClause (x, y, z) = do
  vx <- termExpr x
  vy <- termExpr y
  vz <- termExpr z
  vx .+ vy .+ vz .>=# 1
  pure ()

termExpr :: (MonadIP v c o m, MonadState (M.Map Var v) m) => Term -> m (Expr v)
termExpr (Positive v) = fmap var (getVar v)
termExpr (Negative v) = fmap (scale (-1)) (termExpr (Positive v))

program :: Problem -> AppM (Maybe (M.Map Var Double))
program clauses = do
  mapM_ addClause clauses
  status <- optimizeIP
  case status of
    Error -> liftIO (print "Error") >> pure Nothing
    Infeasible -> pure Nothing
    Unbounded -> pure Nothing
    _ -> do
      vars <- get
      forM_ vars (getName >=> (liftIO . print))
      fmap Just (mapM getVariableValue vars)

printAssignment :: M.Map Var Double -> IO ()
printAssignment vars = void (flip M.traverseWithKey vars $ \k v -> printf "%s=%f\n" (show k) v)

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

We want to make a command-line program that will read in a 3SAT
instance, and either conclude that it is unsatisfiable or output
a witness to satisfiability. Our input format will have one clause per
line, with each variable in the clause being
whitespace-separated. Variables with a leading `-` sign are negated.
