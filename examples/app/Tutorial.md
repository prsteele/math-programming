# Introduction

This is a literate Haskell file.

This document will explain the basics of using the [`math-programming`](github.com/prsteele/math-programming/math-programming)
library, and solve the resulting math programs with the
[`math-programming-glpk`](github.com/prsteele/math-programming/math-programming-glpk)
library.

## Preamble

We add some language extensions,

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
```

and import the libraries we will use

```haskell
import Control.Monad
import Math.Programming
import Math.Programming.Glpk
```

# Solving a very simple optimization problem

As a warm-up, let's solve the problem

$$
    \min cx ~\text{s.t.} x + 1 \ge 9
$$

This is entirely uninteresting from an actual optimization
perspective, but it has just enough to serve as a basic example. We
solve this problem directly.

```haskell
solveSimple :: Double -> IO (Either String Double)
solveSimple c = runGlpk $ do
  -- Get a linear variable
  x <- free

  -- Model "x + 1 >= 9"
  1 *. x .+. con 1 .>= 9

  -- Set the objective
  obj <- minimize (c *. x)

  -- Optimize
  status <- optimizeLP

  case status of
    Unbounded -> pure (Left "unbounded")
    Infeasible -> pure (Left "infeasible")
    Feasible -> pure (Left "merely found a feasible solution")
    Error -> pure (Left "an unknown error occurred")
    Optimal -> fmap Right (getObjectiveValue obj)
```

We can run this for a variety of objective functions with

```haskell
runSimpleSolves :: IO ()
runSimpleSolves =
  forM_ [0, -2, 4] $ \c -> do
    putStr ("with c=" <> show c <> ": ")
    solveSimple c >>= print
```

If we run this, we get

```haskell<!-- example -->
>>> runSimpleSolves
with c=0.0: Right 0.0
with c=-2.0: Left "infeasible"
with c=4.0: Right 32.0
```

This matches our intuition: the problem is clearly feasible for all
$x \ge 8$, so the optimal value will either be $8 c$, or the problem
will be unbounded.

# The `MonadLP'`, `MonadIP`, and `MonadGlpk` typeclasses

These typeclasses encode abstract operations we might perform on a
linear program or integer program, respectively. Note that `MonadLP`
is a superclass of `MonadIP`.

These typeclasses are useful, but they are abstract: we need some
concrete solver backend to actually do any work. We currently have
available one such backend for the GNU Linear Programming Kit, or
GLPK. The `MonadGlpk` typeclass represents the operations this backend
can perform; note that both `MonadLP` and `MonadIP` are superclasses
of `MonadGlpk`. Additionally, there is a concrete type `GlpkT` as an
alternative to the mtl-style `MonadGlpk`.

Let's build an example using as many of these concepts as we can. We
start with a transformation we might be interested in: representing a
"pick $n$" constraint on a collection of binary variables. There's
nothing specific to any solver backend, here, so we can use quite
general types:

```haskell
atMostN :: (MonadIP v c o m, Foldable f) => Int -> f v -> m c
atMostN n xs = lhs .<= fromIntegral n
  where
    lhs = foldr (\x expr -> var x .+. expr) (con 0) xs
```

We can also ask for $n$ binary variables quite generically:

```haskell
getBinary :: MonadIP v c o m => Int -> m [v]
getBinary n = replicateM n binary
```

We can write a simple optimization program using these functions.

```haskell
mipExample :: MonadIP v c o m => Int -> Int -> m (Either String [Bool])
mipExample n k = do
  xs <- getBinary n
  atMostN k xs
  status <- optimizeIP

  case status of
    Unbounded -> pure (Left "unbounded")
    Infeasible -> pure (Left "infeasible")
    Feasible -> pure (Left "merely found a feasible solution")
    Error -> pure (Left "an unknown error occurred")
    Optimal -> fmap Right (mapM f xs)
      where
        f = fmap (>= 0.5) . getVariableValue
```

We can now operate concretely on this program using `runGlpk`; for
example, we can inspect the resulting program using the `writeFormulation`
method of the `MonadGlpk` class.

```haskell
printMIPExample :: IO ()
printMIPExample = runGlpk $ do
  mipExample 10 3
  writeFormulation "/dev/stdout"
```

The result of running this function is

```
\* Problem: Unknown *\

Minimize
 obj: 0 x1

Subject To
 c1: + x10 + x9 + x8 + x7 + x6 + x5 + x4 + x3 + x2 + x1 <= 3

Bounds
 0 <= x1 <= 1
 0 <= x2 <= 1
 0 <= x3 <= 1
 0 <= x4 <= 1
 0 <= x5 <= 1
 0 <= x6 <= 1
 0 <= x7 <= 1
 0 <= x8 <= 1
 0 <= x9 <= 1
 0 <= x10 <= 1

Generals
 x1
 x2
 x3
 x4
 x5
 x6
 x7
 x8
 x9
 x10

End
```

# Using a custom monad stack

See the [`Example3SAT.hs`](./Example3SAT.hs) example of using a custom
type that derives the `MonadLP`, `MonadIP`, and `MonadGlpk` classes.

# Wrapping up

We run all our examples consecutively.

```haskell
main :: IO ()
main = do
  putStrLn "Solving the simplest problem in the world"
  forM_ [0, -2, 4] $ \c -> do
    putStr ("with c=" <> show c <> ": ")
    solveSimple c >>= print

  putStrLn "Printing the sample MIP"
  printMIPExample
```

Running this entire program produces

```

Solving the simplest problem in the world
with c=0.0: Right 0.0
with c=-2.0: Left "infeasible"
with c=4.0: Right 32.0
Printing the sample MIP
\* Problem: Unknown *\

Minimize
 obj: 0 x1

Subject To
 c1: + x10 + x9 + x8 + x7 + x6 + x5 + x4 + x3 + x2 + x1 <= 3

Bounds
 0 <= x1 <= 1
 0 <= x2 <= 1
 0 <= x3 <= 1
 0 <= x4 <= 1
 0 <= x5 <= 1
 0 <= x6 <= 1
 0 <= x7 <= 1
 0 <= x8 <= 1
 0 <= x9 <= 1
 0 <= x10 <= 1

Generals
 x1
 x2
 x3
 x4
 x5
 x6
 x7
 x8
 x9
 x10

End
```
