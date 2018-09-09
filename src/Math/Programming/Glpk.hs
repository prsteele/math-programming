{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Math.Programming.Glpk where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Math.Programming
import Math.Programming.Glpk.Header

newtype Glpk a = Glpk { _runGlpk :: ExceptT GlpkError (ReaderT GlpkEnv IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GlpkEnv
    , MonadError GlpkError
    )

instance LPMonad Glpk Double where
  data Variable Glpk
    = Variable { fromVariable :: GlpkVariable }
    deriving
      ( Eq
      , Ord
      , Show
      )

  data Constraint Glpk
    = Constraint { fromConstraint :: GlpkConstraint }
    deriving
      ( Eq
      , Ord
      , Show
      )

  addVariable = addVariable'
  nameVariable = nameVariable'
  deleteVariable = deleteVariable'
  addConstraint = addConstraint'
  nameConstraint = nameConstraint'
  deleteConstraint = deleteConstraint'
  setObjective = setObjective'
  setSense = setSense'
  optimize = optimize'
  setVariableBounds = setVariableBounds'
  setVariableDomain = setVariableDomain'
  evaluateVariable = evaluateVariable'
  evaluateExpression = evaluateExpression'
  setTimeout = setTimeout'

runGlpk :: Glpk a -> IO (Either GlpkError a)
runGlpk glpk = do
  _ <- glp_term_out glpkOff
  problem <- glp_create_prob

  result <- alloca $ \simplexControlPtr -> do
    glp_init_smcp simplexControlPtr
    env <- GlpkEnv problem
      <$> newIORef []
      <*> newIORef []
      <*> pure simplexControlPtr
    runReaderT (runExceptT (_runGlpk glpk)) env

  glp_delete_prob problem
  return result

data GlpkEnv
  = GlpkEnv
  { _glpkEnvProblem :: Ptr Problem
  , _glpkVariables :: IORef [GlpkVariable]
  , _glpkConstraints :: IORef [GlpkConstraint]
  , _glpkSimplexControl :: Ptr SimplexMethodControlParameters
  }

data NamedRef a
  = NamedRef
    { namedRefId :: Int
    , namedRefRef :: IORef a
    }

instance Eq (NamedRef a) where
  x == y = namedRefId x == namedRefId y

instance Ord (NamedRef a) where
  x <= y = namedRefId x <= namedRefId y

instance Show (NamedRef a) where
  show = show . namedRefId

type GlpkConstraint = NamedRef Row

type GlpkVariable = NamedRef Column

askProblem :: Glpk (Ptr Problem)
askProblem = asks _glpkEnvProblem

askVariablesRef :: Glpk (IORef [GlpkVariable])
askVariablesRef = asks _glpkVariables

askConstraintsRef :: Glpk (IORef [GlpkConstraint])
askConstraintsRef = asks _glpkConstraints

register :: Glpk (IORef [NamedRef a]) -> NamedRef a -> Glpk ()
register askRef x = do
  ref <- askRef
  liftIO $ modifyIORef' ref (x :)

unregister :: (Enum a) => Glpk (IORef [NamedRef a]) -> NamedRef a -> Glpk ()
unregister askRef x =
  let
    decrement (NamedRef _ ref) = modifyIORef' ref pred

    mogrify []                  = return ()
    mogrify (z: zs) | z <= x    = return ()
                    | otherwise = decrement z >> mogrify zs
  in do
    ref <- askRef
    liftIO $ do
      -- Remove the element to be unregistered
      modifyIORef' ref (delete x)

      -- Modify the referenced values that were greater than the
      -- referenced element
      readIORef ref >>= mogrify

data GlpkError
  = UnknownVariable GlpkVariable
  deriving
    ( Show
    )

readColumn :: Variable Glpk -> Glpk Column
readColumn = liftIO . readIORef . namedRefRef . fromVariable

readRow :: Constraint Glpk -> Glpk Row
readRow = liftIO . readIORef . namedRefRef . fromConstraint

addVariable' :: Glpk (Variable Glpk)
addVariable' = do
  problem <- askProblem
  variable <- liftIO $ do
    column <- glp_add_cols problem 1
    columnRef <- newIORef column
    return $ NamedRef (fromIntegral column) columnRef
  register askVariablesRef variable
  return (Variable variable)

nameVariable' :: Variable Glpk -> String -> Glpk ()
nameVariable' variable name = do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ withCString name (glp_set_col_name problem column)

deleteVariable' :: Variable Glpk -> Glpk ()
deleteVariable' variable = do
  problem <- askProblem
  column <- readColumn variable
  liftIO $ allocaGlpkArray [column] (glp_del_cols problem 1)
  unregister askVariablesRef (fromVariable variable)

addConstraint' :: Inequality (Variable Glpk) Double -> Glpk (Constraint Glpk)
addConstraint' (Inequality expr ordering) =
  let
    (terms, constant) =
      let
        LinearExpr terms constant = simplify expr
      in
        (terms, constant)

    constraintType :: GlpkConstraintType
    constraintType = case ordering of
      LT -> glpkLT
      GT -> glpkGT
      EQ -> glpkBounded

    rhs :: CDouble
    rhs = realToFrac (negate constant)

    numVars :: CInt
    numVars = fromIntegral (length terms)

    variables :: [Variable Glpk]
    variables = map fst terms

    coefficients :: [CDouble]
    coefficients = map (realToFrac . snd) terms
  in do
    problem <- askProblem
    columns <- mapM readColumn variables
    constraintId <- liftIO $ do
      row <- glp_add_rows problem 1
      rowRef <- newIORef row
      allocaGlpkArray columns $ \columnArr ->
        allocaGlpkArray coefficients $ \coefficientArr -> do
          glp_set_row_bnds problem row constraintType rhs rhs
          glp_set_mat_row problem row numVars columnArr coefficientArr
      return $ NamedRef (fromIntegral row) rowRef

    register askConstraintsRef constraintId
    return (Constraint constraintId)

nameConstraint' :: Constraint Glpk -> String -> Glpk ()
nameConstraint' constraintId name = do
  problem <- askProblem
  row <- readRow constraintId
  liftIO $ withCString name (glp_set_row_name problem row)

deleteConstraint' :: Constraint Glpk -> Glpk ()
deleteConstraint' constraintId = do
  problem <- askProblem
  row <- readRow constraintId
  liftIO $ allocaGlpkArray [row] (glp_del_rows problem 1)
  unregister askConstraintsRef (fromConstraint constraintId)

setObjective' :: LinearExpr (Variable Glpk) Double -> Glpk ()
setObjective' expr =
  let
    LinearExpr terms constant = simplify expr
  in do
    problem <- askProblem

    -- Set the constant term
    liftIO $ glp_set_obj_coef problem (GlpkInt 0) (realToFrac constant)

    -- Set the variable terms
    forM_ terms $ \(variable, coef) -> do
      column <- readColumn variable
      liftIO $ glp_set_obj_coef problem column (realToFrac coef)

setSense' :: Sense -> Glpk ()
setSense' sense =
  let
    direction = case sense of
      Minimization -> glpkMin
      Maximization -> glpkMax
  in do
    problem <- askProblem
    liftIO $ glp_set_obj_dir problem direction

optimize' :: Glpk SolutionStatus
optimize' =
  let
    convertSuccess status
      | status == glpkOptimal    = Optimal
      | status == glpkFeasible   = Feasible
      | status == glpkInfeasible = Infeasible
      | status == glpkNoFeasible = Infeasible
      | status == glpkUnbounded  = Unbounded
      | otherwise                = Error

    convertResult problem result
      | result == glpkSimplexSuccess =
          glp_get_status problem >>= return . convertSuccess
      | otherwise                    =
          return Error
  in do
    problem <- askProblem
    controlPtr <- asks _glpkSimplexControl
    result <- liftIO $ glp_simplex problem controlPtr
    liftIO $ convertResult problem result

setVariableBounds' :: Variable Glpk -> Bounds Double -> Glpk ()
setVariableBounds' variable bounds =
  let
    (boundType, low, high) = case bounds of
      Free -> (glpkFree, 0, 0)
      NonNegativeReals -> (glpkGT, 0, 0)
      NonPositiveReals -> (glpkLT, 0, 0)
      Interval low high -> (glpkBounded, realToFrac low, realToFrac high)
  in do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ glp_set_col_bnds problem column boundType low high

setVariableDomain' :: Variable Glpk -> Domain -> Glpk ()
setVariableDomain' variable domain =
  let
    vType = case domain of
      Continuous -> glpkContinuous
      Integer -> glpkInteger
      Binary -> glpkBinary
  in do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ glp_set_col_kind problem column vType

evaluateVariable' :: Variable Glpk -> Glpk Double
evaluateVariable' variable = do
    problem <- askProblem
    column <- readColumn variable
    liftIO $ realToFrac <$> glp_get_col_prim problem column

evaluateExpression'
  :: LinearExpr (Variable Glpk) Double
  -> Glpk Double
evaluateExpression' (LinearExpr terms constant) =
  let
    variables = fmap fst terms
    coefs = fmap snd terms
  in do
    values <- mapM evaluate variables
    return $ constant + sum (zipWith (*) values coefs)

setTimeout' :: Double -> Glpk ()
setTimeout' seconds =
  let
    millis :: Integer
    millis = round (seconds * 1000)
  in do
    controlPtr <- asks _glpkSimplexControl
    control <- liftIO (peek controlPtr)
    let control' = control { smcpTimeLimitMillis = fromIntegral millis }
    liftIO (poke controlPtr control')
