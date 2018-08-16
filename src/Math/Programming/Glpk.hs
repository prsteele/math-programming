{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Math.Programming.Glpk where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk.Header

newtype Glpk a = Glpk { runGlpk :: ReaderT GlpkEnv IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GlpkEnv
    )

instance LPMonad Glpk Double where
  makeVariable = makeVariable'
  addConstraint = addConstraint'
  deleteConstraint = deleteConstraint'
  setObjective = setObjective'
  setSense = setSense'
  optimize = optimize'
  setVariableBounds = setVariableBounds'
  setVariableDomain = setVariableDomain'
  evaluateVariable = evaluateVariable'
  evaluateExpression = evaluateExpression'

data GlpkEnv
  = GlpkEnv
    { _glpkEnvProblem :: Ptr Problem
    , _glpkEnvConstraintMap :: IORef ConstraintMap
    }

type ConstraintMap = M.Map ConstraintId Row

getProblem :: Glpk (Ptr Problem)
getProblem = asks _glpkEnvProblem

getConstraintMapRef :: Glpk (IORef ConstraintMap)
getConstraintMapRef = asks _glpkEnvConstraintMap

toCDouble :: Double -> CDouble
toCDouble = realToFrac

toCInt :: Int -> CInt
toCInt = fromIntegral

makeVariable' :: Glpk Variable
makeVariable' = do
  problem <- getProblem
  Column col <- liftIO $ glp_add_cols problem 1
  return (Variable (fromIntegral col))

addConstraint' :: Constraint Variable Double -> Glpk ConstraintId
addConstraint' (Constraint (LinearExpr terms constant) ordering) =
  let
    rhs :: CDouble
    rhs = toCDouble (negate constant)

    numVars :: CInt
    numVars = toCInt (length terms)

    getColumn :: (Variable, Double) -> Column
    getColumn = Column . toCInt . fromVariable . fst

    getCoefficient :: (Variable, Double) -> CDouble
    getCoefficient = toCDouble . snd

    constraintType :: GlpkConstraintType
    constraintType = case ordering of
      LT -> glpkLT
      GT -> glpkGT
      EQ -> glpkBounded

    columns :: [Column]
    columns = fmap getColumn terms

    coefficients :: [CDouble]
    coefficients = fmap getCoefficient terms
  in do
    problem <- getProblem
    row <- liftIO $ do
      row <- glp_add_rows problem 1
      allocaGlpkArray columns $ \columnArr ->
        allocaGlpkArray coefficients $ \coefArr -> do
          glp_set_row_bnds problem row constraintType rhs rhs
          glp_set_mat_row problem row numVars columnArr coefArr
      return row

    addConstraintToMap row

deleteConstraint' constraintId = do
  constraintMap <- getConstraintMapRef >>= liftIO . readIORef
  case M.lookup constraintId constraintMap of
    Nothing -> return ()
    Just row -> do
      problem <- getProblem
      liftIO $ do
        rows <- mallocGlpkArray [row]
        glp_del_rows problem 1 rows
        free (fromGlpkArray rows)

      deleteConstraintFromMap constraintId row


setObjective' (LinearExpr terms constant) = do
  problem <- getProblem

  -- Set the constant term
  liftIO $ glp_set_obj_coef problem (Column 0) (toCDouble constant)

  -- Set the variable terms
  liftIO $ forM_ terms $ \(Variable column, coef) ->
    glp_set_obj_coef problem (Column (toCInt column)) (toCDouble coef)

setSense' sense =
  let
    direction = case sense of
      Minimization -> glpkMin
      Maximization -> glpkMax
  in do
    problem <- getProblem
    liftIO $ glp_set_obj_dir problem direction

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
    problem <- getProblem
    result <- liftIO $ glp_simplex problem nullPtr
    liftIO $ convertResult problem result

setVariableBounds' (Variable variable) bounds =
  let
    column = Column (toCInt variable)

    (boundType, low, high) = case bounds of
      Free -> (glpkFree, 0, 0)
      NonNegativeReals -> (glpkGT, 0, 0)
      NonPositiveReals -> (glpkLT, 0, 0)
      Interval low high -> (glpkBounded, low, high)

  in do
    problem <- getProblem
    liftIO $ glp_set_col_bnds problem column boundType (toCDouble low) (toCDouble high)

setVariableDomain' (Variable variable) domain =
  let
    column = Column (toCInt variable)
    vType = case domain of
      Continuous -> glpkContinuous
      Integer -> glpkInteger
      Binary -> glpkBinary
  in do
    problem <- getProblem
    liftIO $ glp_set_col_kind problem column vType

evaluateVariable' (Variable variable) =
  let
    column = Column (toCInt variable)
  in do
    problem <- getProblem
    liftIO $ realToFrac <$> glp_get_col_prim problem column

evaluateExpression' (LinearExpr terms constant) =
  let
    variables = fmap fst terms
    coefs = fmap snd terms
  in do
    values <- mapM evaluate variables
    return $ constant + sum (zipWith (*) values coefs)

addConstraintToMap :: Row -> Glpk ConstraintId
addConstraintToMap row =
  let
    constraintId = ConstraintId . fromIntegral . fromRow $ row
    insert = M.insert constraintId row
  in do
    ref <- getConstraintMapRef
    liftIO $ modifyIORef' ref insert
    return constraintId

deleteConstraintFromMap :: ConstraintId -> Row -> Glpk ()
deleteConstraintFromMap constraintId (Row removed) =
  let
    decrement :: Row -> Row
    decrement (Row toUpdate) | removed < toUpdate = Row (toUpdate - 1)
                             | otherwise          = Row toUpdate
  in do
    ref <- getConstraintMapRef
    liftIO $ do
      constraintMap <- readIORef ref
      writeIORef ref $ fmap decrement (M.delete constraintId constraintMap)
