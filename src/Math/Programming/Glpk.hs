{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Math.Programming.Glpk where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk.Header

newtype Glpk a = Glpk { runGlpk :: ExceptT GlpkError (ReaderT GlpkEnv IO) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader GlpkEnv
    , MonadError GlpkError
    )

instance LPMonad Glpk Double where
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

data GlpkError
  = UnknownVariable Variable
  deriving
    ( Read
    , Show
    )

addVariable' :: Glpk Variable
addVariable' = do
  problem <- getProblem
  column <- liftIO $ glp_add_cols problem 1
  let variable = Variable . fromIntegral . fromGlpkInt $ column
  addToMap getVariableMapRef variable column
  return variable

nameVariable' :: Variable -> String -> Glpk ()
nameVariable' variable name =
  let
    column = GlpkInt . toCInt . fromVariable $ variable
  in do
    problem <- getProblem
    liftIO $ withCString name (glp_set_col_name problem column)

deleteVariable' :: Variable -> Glpk ()
deleteVariable' variable = do
  mCol <- lookupFromMap getVariableMapRef variable
  case mCol of
    Nothing -> return ()
    Just col -> do
      problem <- getProblem
      liftIO $ allocaGlpkArray [col] (glp_del_cols problem 1)
      deleteFromMap getVariableMapRef variable col

addConstraint' :: Constraint Variable Double -> Glpk ConstraintId
addConstraint' (Constraint (LinearExpr terms constant) ordering) =
  let
    rhs :: CDouble
    rhs = toCDouble (negate constant)

    numVars :: CInt
    numVars = toCInt (length terms)

    getColumn :: (Variable, Double) -> Column
    getColumn = GlpkInt . toCInt . fromVariable . fst

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

    let constraintId = ConstraintId . fromIntegral . fromGlpkInt $ row
    addToMap getConstraintMapRef constraintId row
    return constraintId

nameConstraint' :: ConstraintId -> String -> Glpk ()
nameConstraint' constraintId name =
  let
    row = GlpkInt . toCInt . fromConstraintId $ constraintId
  in do
    problem <- getProblem
    mRow <- lookupFromMap getConstraintMapRef constraintId
    case mRow of
      Just row -> liftIO $ withCString name (glp_set_row_name problem row)
      Nothing -> return ()

deleteConstraint' constraintId = do
  mRow <- lookupFromMap getConstraintMapRef constraintId
  case mRow of
    Nothing -> return ()
    Just row -> do
      problem <- getProblem
      liftIO $ allocaGlpkArray [row] (glp_del_rows problem 1)
      deleteFromMap getConstraintMapRef constraintId row

setObjective' (LinearExpr terms constant) = do
  problem <- getProblem

  -- Set the constant term
  liftIO $ glp_set_obj_coef problem (GlpkInt 0) (toCDouble constant)

  -- Set the variable terms
  liftIO $ forM_ terms $ \(Variable column, coef) ->
    glp_set_obj_coef problem (GlpkInt (toCInt column)) (toCDouble coef)

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
    column = GlpkInt (toCInt variable)

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
    column = GlpkInt (toCInt variable)
    vType = case domain of
      Continuous -> glpkContinuous
      Integer -> glpkInteger
      Binary -> glpkBinary
  in do
    problem <- getProblem
    liftIO $ glp_set_col_kind problem column vType

evaluateVariable' (Variable variable) =
  let
    column = GlpkInt (toCInt variable)
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

data GlpkEnv
  = GlpkEnv
    { _glpkEnvProblem :: Ptr Problem
    , _glpkEnvConstraintMap :: IORef ConstraintMap
    , _glpkEnvVariableMap :: IORef VariableMap
    }

type ConstraintMap = M.Map ConstraintId Row
type VariableMap = M.Map Variable Column

deleteFromMap
  :: (Ord a, Ord b, Num b)
  => Glpk (IORef (M.Map a b))
  -- ^ An action to retrieve a reference to the map
  -> a
  -- ^ The key to remove
  -> b
  -- ^ The value being removed
  -> Glpk ()
deleteFromMap getMapRef removedKey removedValue =
  let
    decrement z = if removedValue < z
                  then z - 1
                  else z
  in do
    ref <- getMapRef
    liftIO $ do
      mapping <- readIORef ref
      writeIORef ref $ fmap decrement (M.delete removedKey mapping)

lookupFromMap
  :: (Ord a, Ord b, Num b)
  => Glpk (IORef (M.Map a b))
  -> a
  -> Glpk (Maybe b)
lookupFromMap getMapRef key = do
  mapping <- getMapRef >>= liftIO . readIORef
  pure $ M.lookup key mapping

addToMap
  :: (Ord a, Ord b, Num b)
  => Glpk (IORef (M.Map a b))
  -> a
  -> b
  -> Glpk ()
addToMap getMapRef key value = do
  ref <- getMapRef
  liftIO $ modifyIORef' ref (M.insert key value)

getProblem :: Glpk (Ptr Problem)
getProblem = asks _glpkEnvProblem

getConstraintMapRef :: Glpk (IORef ConstraintMap)
getConstraintMapRef = asks _glpkEnvConstraintMap

getConstraintMap :: Glpk ConstraintMap
getConstraintMap = getConstraintMapRef >>= liftIO . readIORef

getVariableMapRef :: Glpk (IORef VariableMap)
getVariableMapRef = asks _glpkEnvVariableMap

getVariableMap :: Glpk VariableMap
getVariableMap = getVariableMapRef >>= liftIO . readIORef

toColumns :: [Variable] -> Glpk [Column]
toColumns variables =
  let
    eLookup :: M.Map Variable Column -> Variable -> Either GlpkError Column
    eLookup m v = case M.lookup v m of
      Nothing -> Left (UnknownVariable v)
      Just column -> Right column
  in do
    varMap <- getVariableMap
    liftEither . sequence . fmap (eLookup varMap) $ variables

toCDouble :: Double -> CDouble
toCDouble = realToFrac

toCInt :: Int -> CInt
toCInt = fromIntegral
