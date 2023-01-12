# Introduction

This is a literate Haskell file.

We want to create a [Sudoku](https://en.wikipedia.org/wiki/Sudoku)
solver using mixed-integer programming.

## Preamble

We define relevant language pragmas,

```haskell
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
```

and import the necessary libraries.

```haskell
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Math.Programming
import Math.Programming.Glpk
import Text.Printf
import System.Exit
```

## Formulation

We'll focus on a 9x9 board, but the formulation generalizes in a
straightforward fashion to any size and shape board. Define $[n] = \{1, 2, \ldots, n\}$.


### Variables

We introduce binary variables $x_{i,j,k} ~ \forall (i, j, k) \in [9] \times [9] \times [9]$,
where we intend for $x_{i,j,k} = 1$ if and only if the contents of the
cell at row $i$ and column $j$ is the value $k$.

Let's declare types for rows, columns, values of cells, and logical
MIP variables.

```haskell
newtype Row = Row Int
  deriving (Eq, Ord)


newtype Col = Col Int
  deriving (Eq, Ord)


newtype Val = Val Int
  deriving (Eq, Ord)

newtype Var = Var (Row, Col, Val)
  deriving (Show, Eq, Ord)
```

We want more succinct `Show` instances to help out MIP formulation writing:

```haskell
instance Show Row where
  show (Row x) = "r" <> show x

instance Show Col where
  show (Col x) = "c" <> show x

instance Show Val where
  show (Val x) = "v" <> show x
```

It will be very convenient to have type-specialized indices available:

```haskell
allRows :: [Row]
allRows = fmap Row [1..9]

allCols :: [Col]
allCols = fmap Col [1..9]

allVals :: [Val]
allVals = fmap Val [1..9]
```

Finally, we'll want to keep track of the variables we create; the
following constraint synonym will be helpful.

```haskell
type SudokuM v c o m =
  ( MonadState (M.Map Var v) m,
    MonadLP v c o m,
    MonadIP v c o m,
    MonadIO m
  )
```

We can now create a map containing all our variables.

```haskell
createVariables :: SudokuM v c o m => m ()
createVariables =
  let varIxs = Var <$> ((,,) <$> allRows <*> allCols <*> allVals)
      varName (Var (row, col, val)) =
        "x_" <> tshow row <> tshow col <> tshow val
   in do
        vars <- forM varIxs $ \varIx ->
          binary `withVariableName` (varName varIx)
        put $ M.fromList (zip varIxs vars)

tshow :: Show a => a -> T.Text
tshow = T.pack . show
```

### Constraints

First, we constrain each cell to have a single value with constraints

$$
  \sum_{k \in [9]} x_{i, j, k} = 1 \quad \forall (i, j) \in [9] \times [9].
$$

```haskell
createUniqueConstraints :: SudokuM v c o m => m ()
createUniqueConstraints = do
  vars <- get
  forM_ allRows $ \row ->
    forM_ allCols $ \col -> do
      let lhs = vsum [vars M.! Var (row, col, val) | val <- allVals]
          name = "unique_" <> tshow row <> tshow col
      (lhs .== 1) `withConstraintName` name
```

We can force each row to have one of each value with the constraints

$$
  \sum_{j \in [9]} \sum_{k \in [9]} x_{i, j, k} = 1 \quad \forall i
  \in [9].
$$

```haskell
createRowConstraints :: SudokuM v c o m => m ()
createRowConstraints = do
  vars <- get
  forM_ allRows $ \row -> do
    forM_ allVals $ \val -> do
      let lhs = vsum [vars M.! Var (row, col, val) | col <- allCols]
          name = "row_" <> tshow row <> tshow val
      (lhs .== 1) `withConstraintName` name
```

Likewise, we force each column to have one of each value with the constraints

$$
  \sum_{i \in [9]} \sum_{k \in [9]} x_{i, j, k} = 1 \quad \forall j
  \in [9].
$$

```haskell
createColConstraints :: SudokuM v c o m => m ()
createColConstraints = do
  vars <- get
  forM_ allCols $ \col -> do
    forM_ allVals $ \val -> do
      let lhs = vsum [vars M.! Var (row, col, val) | row <- allRows]
          name = "col_" <> tshow col <> tshow val
      (lhs .== 1) `withConstraintName` name
```

If we define $B_1, \ldots, B_9$ as the pairs of coordinates
defining the nine squares,

```haskell
square :: Int -> [(Row, Col)]
square i =
  let squareRow = ((i - 1) `div` 3) * 3 + 1
      squareCol = ((i - 1) `mod` 3) * 3 + 1
   in [ (Row r, Col c)
      | r <- [squareRow..squareRow + 2]
      , c <- [squareCol..squareCol + 2]
      ]
```

we enforce that each 3x3 square has one of
each value with the constraints

$$
  \sum_{i, j \in B_\ell} \sum_{k \in [9]} x_{i, j, k} = 1 \quad
  \forall \ell \in [9].
$$

```haskell
createSquareConstraints :: SudokuM v c o m => m ()
createSquareConstraints = do
  vars <- get
  forM_ [1..9] $ \ell -> do
    forM_ allVals $ \val -> do
      let lhs = vsum [ vars M.! Var (row, col, val) | (row, col) <- square ell]
          name = "square_" <> tshow ell
      (lhs .== 1) `withConstraintName` name
```

Finally, we set our initial conditions with the constraints

$$
  x_{i, j, k} = 1
$$

for each initially-filled cell $(i, j)$ with value $k$.

```haskell
createInitialConditions :: SudokuM v c o m => M.Map (Row, Col) Val -> m ()
createInitialConditions filled = do
  vars <- get
  flip M.traverseWithKey filled $ \(row, col) val -> do
    let lhs = 1 *. (vars M.! Var (row, col, val))
        name = "initial_" <> tshow row <> tshow col <> tshow val
     in (lhs .== 1) `withConstraintName` name
  pure ()
```

## Parsing problem input

We'll consume a very simple input format. Our input will be 9 lines of
text where each line is 9 characters long. Characters 1--9 will be
interpreted as initial conditions; all other characters are
interpreted as blanks. For example, the empty puzzle (with no initial
conditions) might be represented as

```
.........
.........
.........
.........
.........
.........
.........
.........
.........
```

The first puzzle depicted in the [Wikipedia
article](https://en.wikipedia.org/wiki/Sudoku), as of this writing,
can be represented as

```
53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79
```

```haskell
parseInput :: String -> Either String (M.Map (Row, Col) Val)
parseInput x =
  let rows = lines x

      parseRow :: Row -> String -> Either String [((Row, Col), Val)]
      parseRow row rowData =
        if length rowData /= 9
        then Left ("row length is not 9: " <> rowData)
        else Right . discardNothings $ [((row, col), f c) | (col, c) <- zip allCols rowData]

      discardNothings :: [(a, Maybe b)] -> [(a, b)]
      discardNothings [] = []
      discardNothings (x@(u, Nothing):xs) = discardNothings xs
      discardNothings (x@(u, Just v):xs) = (u, v) : discardNothings xs

      f '1' = Just (Val 1)
      f '2' = Just (Val 2)
      f '3' = Just (Val 3)
      f '4' = Just (Val 4)
      f '5' = Just (Val 5)
      f '6' = Just (Val 6)
      f '7' = Just (Val 7)
      f '8' = Just (Val 8)
      f '9' = Just (Val 9)
      f _ = Nothing

   in if length rows /= 9
      then Left ("Only " <> show (length rows) <> " rows in input")
      else do
        entries <- sequence (zipWith parseRow allRows rows)
        pure (M.fromList (concat entries))
```

## Solving a puzzle

We've defined functions to create variables and constraints (and we
have no objective function,) so we can now create our MIP.

```haskell
program :: SudokuM v c o m => M.Map (Row, Col) Val -> m ()
program initialConditions = do
  createVariables
  createUniqueConstraints
  createRowConstraints
  createColConstraints
  createSquareConstraints
  createInitialConditions initialConditions
```

If we find an optimal variable assignment, we need to convert it back to a solved puzzle.

```haskell
puzzleFromVars :: M.Map Var Double -> M.Map (Row, Col) Val
puzzleFromVars opt = M.fromList [((row, col), f row col) | row <- allRows, col <- allCols]
  where
    f row col = Val total
      where
        total = sum [ round (M.findWithDefault 0 (Var (row, col, Val v)) opt) * v
                    | Val v <- allVals
                    ]
```

We can now attempt to solve the puzzle.

```haskell
solve :: M.Map (Row, Col) Val -> IO (Maybe (M.Map (Row, Col) Val))
solve initialConditions =
  runGlpk . flip evalStateT M.empty $ do
    program initialConditions
    status <- optimizeIP

    case status of
      Error -> liftIO (print "Error encountered during solve" >> exitFailure)
      Unbounded -> liftIO (print "Unbounded problem instance" >> exitFailure)
      Infeasible -> pure Nothing

      -- This matches both 'Optimal' and 'Feasible', which are equivalent
      -- in this formulation
      _ -> get >>= traverse getVariableValue >>= pure . Just . puzzleFromVars
```

Finally, we'll want to be able to print solved puzzles.

```haskell
formatSolution :: M.Map (Row, Col) Val -> String
formatSolution sol = unlines rows
  where
    rows = fmap formatRow allRows

    f :: Maybe Val -> String
    f Nothing = "."
    f (Just (Val v)) = show v

    formatRow row = concat [f (M.lookup (row, col) sol) | col <- allCols]
```

## Running

```haskell
main :: IO ()
main = do
  parsed <- fmap parseInput getContents
  case parsed of
    Left err -> putStrLn err
    Right initialConditions -> do
      mSolution <- solve initialConditions
      case mSolution of
        Nothing -> putStrLn "Infeasible puzzle"
        Just sol -> putStrLn (formatSolution sol)
```

## Examples

Let's solve the two examples we gave above. Let's put the empty puzzle in `empty.puzzle`, and the Wikipedia puzzle in `wiki.puzzle`. Solve the empty puzzle, we get

```
$ ./result/bin/sudoku < empty.puzzle
159643782
836275194
274198653
745936821
983721465
612854379
568419237
497382516
321567948
```

Note that the actual values could vary depending on how the GLPK
solver happens to make choices internally.

If we solve the Wikipedia example, we get

```
$ ./result/bin/sudoku < wiki.puzzle
534678912
672195348
198342567
859761423
426853791
713924856
961537284
287419635
345286179
```

In both cases the resulting filled puzzles are valid.

What happens if we provide an unsolvable puzzle, like the following?

```
.........
.........
.........
.........
......22.
.........
.........
.........
.........
```

Save this to `infeasible.puzzle`, and we get

```
$ ./result/bin/sudoku < infeasible.puzzle
Infeasible puzzle
```
