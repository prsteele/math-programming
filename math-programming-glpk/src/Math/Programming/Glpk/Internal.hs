{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module contains the full definitions backing the simplified API
-- exposed in 'Math.Programming.Glpk'.
module Math.Programming.Glpk.Internal where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor
import Data.IORef
import qualified Data.Text as T
import Data.Typeable
import Data.Void
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Math.Programming
import Math.Programming.Glpk.Header
import UnliftIO.Concurrent
import UnliftIO.Exception

-- | A reference to a GLPK variable.
type GlpkVariable = GlpkPtr Column

-- | A reference to a GLPK constraint.
type GlpkConstraint = GlpkPtr Row

-- | A placeholder for an objective.
--
-- GLPK supports only single-objective problems, and so no indices
-- need to be stored.
newtype GlpkObjective = GlpkObjective ()

-- | An interface to the low-level GLPK API.
--
-- High-level solver settings can be modified by altering the
-- 'SimplexMethodControlParameters' and 'MIPControlParameters' values
-- for LP and IP solves, respectively.
data GlpkEnv = GlpkEnv
  { -- | A pointer to the Problem object. Most GLPK routines take this
    -- as the first argument.
    _glpkEnvProblem :: Ptr Problem,
    -- | The variables in the model
    _glpkVariables :: IORef [GlpkVariable],
    -- | The next unique ID to assign to a variable.
    _glpkNextVariableId :: IORef Integer,
    -- | The constraints in the model
    _glpkConstraints :: IORef [GlpkConstraint],
    -- | The next unique ID to assign to a variable.
    _glpkNextConstraintId :: IORef Integer,
    -- | The control parameters for the simplex method
    _glpkSimplexControl :: IORef SimplexMethodControlParameters,
    -- | The control parameters for the MIP solver
    _glpkMIPControl :: IORef (MIPControlParameters Void),
    -- | The type of the last solve. This is needed to know whether to
    -- retrieve simplex, interior point, or MIP solutions.
    _glpkLastSolveType :: IORef (Maybe SolveType)
  }

-- | A pointer to an IORef, labeled by a unique value.
--
-- Although variables and constraints are labeled interally as
-- integers, GLPK will re-use integer labels when variables or
-- constraints are deleted and new ones are added. To ensure that we
-- do not confuse objects, we mark all created variables and
-- constraints with a unique integer value.
data GlpkPtr a = GlpkPtr
  { -- | The unique identifier of this reference.
    --
    -- Objects of this type, when generated, will have a fresh '_refId' value.
    _glpkPtrId :: Integer,
    -- | Whether this reference has been deleted from the problem.
    _glpkPtrDeleted :: IORef Bool,
    -- | The referenced object.
    _glpkPtrRef :: IORef a
  }

instance Eq (GlpkPtr a) where
  (GlpkPtr x _ _) == (GlpkPtr y _ _) = x == y

instance Ord (GlpkPtr a) where
  compare (GlpkPtr x _ _) (GlpkPtr y _ _) = compare x y

-- | An error that GLPK can encounter.
data GlpkException
  = UnknownVariable
  | UnknownCode T.Text CInt
  | GlpkFailure T.Text
  deriving
    ( Show,
      Typeable
    )

instance Exception GlpkException

-- | An environment to solve math programs using GLPK.
newtype Glpk a = Glpk {_runGlpk :: ReaderT GlpkEnv IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader GlpkEnv
    )

instance LPMonad GlpkVariable GlpkConstraint GlpkObjective Glpk where
  addVariable = addVariable'
  deleteVariable = deleteVariable'
  getVariableName = getVariableName'
  setVariableName = setVariableName'
  getVariableValue = getVariableValue'
  getBounds = getVariableBounds'
  setBounds = setVariableBounds'
  addConstraint = addConstraint'
  deleteConstraint = deleteConstraint'
  getConstraintName = getConstraintName'
  setConstraintName = setConstraintName'
  getConstraintValue = getDualValue

  addObjective = addObjective'
  getObjectiveValue = getObjectiveValue'
  getSense = getSense'
  setSense = setSense'
  getObjectiveName = getObjectiveName'
  setObjectiveName = setObjectiveName'

  getTimeout = getTimeout'
  setTimeout = setTimeout'
  optimizeLP = optimizeLP'

instance IPMonad GlpkVariable GlpkConstraint GlpkObjective Glpk where
  getDomain = getVariableDomain'
  setDomain = setVariableDomain'
  getRelativeMIPGap = getRelativeMIPGap'
  setRelativeMIPGap = setRelativeMIPGap'
  optimizeIP = optimizeIP'

withGlpkErrorHook :: (Ptr a -> IO CInt) -> Ptr a -> IO b -> IO b
withGlpkErrorHook hook ptr actions =
  bracket (mkHaskellErrorHook hook) freeHaskellFunPtr $ \hookPtr -> do
    glp_error_hook hookPtr ptr
    actions

removeGlpkErrorHook :: IO ()
removeGlpkErrorHook = glp_error_hook nullFunPtr nullPtr

runGlpk :: Glpk a -> IO a
runGlpk program =
  let withGlpkEnv actions =
        bracket glp_init_env (const glp_free_env) $ \case
          0 -> actions
          1 -> throwIO (GlpkFailure "GLPK already initialized")
          2 -> throwIO (GlpkFailure "GLPK failed to initialize; not enough memory")
          3 -> throwIO (GlpkFailure "GLPK failed to initialize; unsupported programming model")
          r -> throwIO (GlpkFailure ("GLPK failed to initialize; unknown status code " <> T.pack (show r)))
   in runInBoundThread $
        withGlpkEnv $
          flip finally removeGlpkErrorHook $
            withGlpkErrorHook (const glp_free_env) nullPtr $
              runGlpk' program

getDefaultSimplexControlParameters :: IO SimplexMethodControlParameters
getDefaultSimplexControlParameters = do
  params <- alloca $ \simplexControlPtr -> do
    glp_init_smcp simplexControlPtr
    peek simplexControlPtr

  -- Turn on presolve. Users can simply turn this off before the first
  -- optimization call if they desire.
  pure (params {smcpPresolve = glpkPresolveOn})

getDefaultMIPControlParameters :: IO (MIPControlParameters Void)
getDefaultMIPControlParameters = do
  params <- alloca $ \mipControlPtr -> do
    glp_init_iocp mipControlPtr
    peek mipControlPtr

  -- Turn on presolve. Users can simply turn this off before the first
  -- optimization call if they desire.
  pure (params {iocpPresolve = glpkPresolveOn})

runGlpk' :: Glpk a -> IO a
runGlpk' glpk = do
  -- Turn off terminal output. If we don't, users won't be able to
  -- inhibit terminal output generated from our setup.
  _ <- glp_term_out glpkOff

  bracket glp_create_prob glp_delete_prob $ \problem -> do
    env <-
      GlpkEnv problem
        <$> newIORef []
        <*> newIORef 0
        <*> newIORef []
        <*> newIORef 0
        <*> (getDefaultSimplexControlParameters >>= newIORef)
        <*> (getDefaultMIPControlParameters >>= newIORef)
        <*> newIORef Nothing

    runReaderT (_runGlpk glpk) env

data SolveType = LP | MIP | InteriorPoint

askProblem :: Glpk (Ptr Problem)
askProblem = asks _glpkEnvProblem

askVariablesRef :: Glpk (IORef [GlpkVariable])
askVariablesRef = asks _glpkVariables

askConstraintsRef :: Glpk (IORef [GlpkConstraint])
askConstraintsRef = asks _glpkConstraints

register :: GlpkPtr a -> IORef [GlpkPtr a] -> Glpk ()
register newPtr ptrRefs = do
  liftIO $ modifyIORef' ptrRefs (newPtr :)

unregister :: Integral a => GlpkPtr a -> IORef [GlpkPtr a] -> Glpk ()
unregister deletedPtr ptrsRef =
  let update removed (GlpkPtr _ _ ptr) = do
        z <- readIORef ptr
        when (z > removed) $
          modifyIORef' ptr pred
   in liftIO $ do
        -- If the reference was already deleted, do nothing
        deleted <- readIORef (_glpkPtrDeleted deletedPtr)
        unless deleted $ do
          -- Mark deletion
          writeIORef (_glpkPtrDeleted deletedPtr) True

          -- Remove the element to be unregistered
          modifyIORef' ptrsRef (filter ((/= _glpkPtrId deletedPtr) . _glpkPtrId))

          -- Modify the referenced values that were greater than the
          -- referenced element.
          deletedId <- readIORef (_glpkPtrRef deletedPtr)
          ptrs <- readIORef ptrsRef
          mapM_ (update deletedId) ptrs

readColumn :: GlpkVariable -> Glpk Column
readColumn = liftIO . readIORef . _glpkPtrRef

readRow :: GlpkConstraint -> Glpk Row
readRow = liftIO . readIORef . _glpkPtrRef

addVariable' :: Glpk GlpkVariable
addVariable' = do
  problem <- askProblem
  variable <- liftIO $ do
    column <- glp_add_cols problem 1
    glp_set_col_bnds problem column glpkFree 0 0
    GlpkPtr (fromIntegral column)
      <$> newIORef False
      <*> newIORef column

  asks _glpkVariables >>= register variable
  pure variable

setVariableName' :: GlpkVariable -> T.Text -> Glpk ()
setVariableName' variable name = do
  problem <- askProblem
  column <- readColumn variable
  liftIO $ withCText name (glp_set_col_name problem column)

getVariableName' :: GlpkVariable -> Glpk T.Text
getVariableName' variable = do
  problem <- askProblem
  column <- readColumn variable
  name <- liftIO $ glp_get_col_name problem column >>= peekCString
  pure (T.pack name)

deleteVariable' :: GlpkVariable -> Glpk ()
deleteVariable' variable = do
  problem <- askProblem
  column <- readColumn variable
  liftIO $ allocaGlpkArray [column] (glp_del_cols problem 1)
  asks _glpkVariables >>= unregister variable

addConstraint' :: Inequality (Expr GlpkVariable) -> Glpk GlpkConstraint
addConstraint' (Inequality ordering lhs rhs) =
  let LinearExpression terms constant = simplify (lhs .-. rhs) :: Expr GlpkVariable

      constraintType :: GlpkConstraintType
      constraintType = case ordering of
        LT -> glpkLT
        GT -> glpkGT
        EQ -> glpkFixed

      constraintRhs :: CDouble
      constraintRhs = realToFrac (negate constant)

      numVars :: CInt
      numVars = fromIntegral (length terms)

      variables :: [GlpkVariable]
      variables = map snd terms

      coefficients :: [CDouble]
      coefficients = map (realToFrac . fst) terms
   in do
        problem <- askProblem
        columns <- mapM readColumn variables
        constraintPtr <- liftIO $ do
          row <- glp_add_rows problem 1
          allocaGlpkArray columns $ \columnArr ->
            allocaGlpkArray coefficients $ \coefficientArr -> do
              glp_set_row_bnds problem row constraintType constraintRhs constraintRhs
              glp_set_mat_row problem row numVars columnArr coefficientArr

          GlpkPtr (fromIntegral row)
            <$> newIORef False
            <*> newIORef row

        asks _glpkConstraints >>= register constraintPtr
        pure constraintPtr

setConstraintName' :: GlpkConstraint -> T.Text -> Glpk ()
setConstraintName' constraintId name = do
  problem <- askProblem
  row <- readRow constraintId
  liftIO $ withCText name (glp_set_row_name problem row)

getConstraintName' :: GlpkConstraint -> Glpk T.Text
getConstraintName' constraint = do
  problem <- askProblem
  row <- readRow constraint
  name <- liftIO $ glp_get_row_name problem row >>= peekCString
  pure (T.pack name)

getDualValue :: GlpkConstraint -> Glpk Double
getDualValue constraint = do
  problem <- askProblem
  row <- readRow constraint
  fmap realToFrac . liftIO $ glp_get_row_dual problem row

deleteConstraint' :: GlpkConstraint -> Glpk ()
deleteConstraint' constraint = do
  problem <- askProblem
  row <- readRow constraint
  liftIO $ allocaGlpkArray [row] (glp_del_rows problem 1)
  asks _glpkConstraints >>= unregister constraint

addObjective' :: Expr GlpkVariable -> Glpk GlpkObjective
addObjective' expr =
  let LinearExpression terms constant = simplify expr
   in do
        problem <- askProblem

        -- Set the constant term
        liftIO $ glp_set_obj_coef problem (GlpkInt 0) (realToFrac constant)

        -- Set the variable terms
        forM_ terms $ \(coef, variable) -> do
          column <- readColumn variable
          liftIO $ glp_set_obj_coef problem column (realToFrac coef)

        pure (GlpkObjective ())

getObjectiveName' :: GlpkObjective -> Glpk T.Text
getObjectiveName' _ = do
  problem <- askProblem
  name <- liftIO $ glp_get_obj_name problem >>= peekCString
  pure (T.pack name)

setObjectiveName' :: GlpkObjective -> T.Text -> Glpk ()
setObjectiveName' _ name = do
  problem <- askProblem
  liftIO $ withCText name (glp_set_obj_name problem)

getSense' :: GlpkObjective -> Glpk Sense
getSense' _ = do
  problem <- askProblem
  direction <- liftIO $ glp_get_obj_dir problem
  if direction == glpkMin
    then pure Minimization
    else pure Maximization

setSense' :: GlpkObjective -> Sense -> Glpk ()
setSense' _ sense =
  let direction = case sense of
        Minimization -> glpkMin
        Maximization -> glpkMax
   in do
        problem <- askProblem
        liftIO $ glp_set_obj_dir problem direction

getObjectiveValue' :: GlpkObjective -> Glpk Double
getObjectiveValue' _ = do
  problem <- askProblem
  lastSolveRef <- asks _glpkLastSolveType
  lastSolve <- (liftIO . readIORef) lastSolveRef
  fmap realToFrac . liftIO $ case lastSolve of
    Just MIP -> glp_mip_obj_val problem
    Just LP -> glp_get_obj_val problem
    Just InteriorPoint -> glp_ipt_obj_val problem
    Nothing -> glp_get_obj_val problem -- There's been no solve, so who cares

optimizeLP' :: Glpk SolutionStatus
optimizeLP' = do
  -- Note that we've run an LP solve
  solveTypeRef <- asks _glpkLastSolveType
  liftIO $ writeIORef solveTypeRef (Just LP)

  -- Run Simplex
  problem <- askProblem
  controlRef <- asks _glpkSimplexControl
  liftIO $ do
    control <- readIORef controlRef
    alloca $ \controlPtr -> do
      poke controlPtr control
      _ <- glp_simplex problem controlPtr
      glp_get_status problem Data.Functor.<&> solutionStatus

optimizeIP' :: Glpk SolutionStatus
optimizeIP' = do
  -- Note that we've run a MIP solve
  solveTypeRef <- asks _glpkLastSolveType
  liftIO $ writeIORef solveTypeRef (Just MIP)

  problem <- askProblem
  controlRef <- asks _glpkMIPControl
  liftIO $ do
    control <- readIORef controlRef
    alloca $ \controlPtr -> do
      poke controlPtr control
      _ <- glp_intopt problem controlPtr
      glp_mip_status problem Data.Functor.<&> solutionStatus

setVariableBounds' :: GlpkVariable -> Bounds -> Glpk ()
setVariableBounds' variable bounds =
  let (boundType, cLow, cHigh) = case bounds of
        Free -> (glpkFree, 0, 0)
        NonNegativeReals -> (glpkGT, 0, 0)
        NonPositiveReals -> (glpkLT, 0, 0)
        Interval low high -> (glpkBounded, realToFrac low, realToFrac high)
   in do
        problem <- askProblem
        column <- readColumn variable
        liftIO $ glp_set_col_bnds problem column boundType cLow cHigh

getVariableBounds' :: GlpkVariable -> Glpk Bounds
getVariableBounds' variable =
  let boundsFor lb ub
        | lb == - maxCDouble && ub == maxCDouble = Free
        | lb == - maxCDouble && ub == 0.0 = NonPositiveReals
        | lb == 0.0 && ub == maxCDouble = NonNegativeReals
        | otherwise = Interval lb' ub'
        where
          lb' = realToFrac lb
          ub' = realToFrac ub
   in do
        problem <- askProblem
        column <- readColumn variable
        lb <- liftIO (glp_get_col_lb problem column)
        ub <- liftIO (glp_get_col_ub problem column)
        return (boundsFor lb ub)

setVariableDomain' :: GlpkVariable -> Domain -> Glpk ()
setVariableDomain' variable domain =
  let vType = case domain of
        Continuous -> glpkContinuous
        Integer -> glpkInteger
        Binary -> glpkBinary
   in do
        problem <- askProblem
        column <- readColumn variable
        liftIO $ glp_set_col_kind problem column vType

getVariableDomain' :: GlpkVariable -> Glpk Domain
getVariableDomain' variable =
  let getDomain' :: GlpkVariableType -> Glpk Domain
      getDomain' vType | vType == glpkContinuous = return Continuous
      getDomain' vType | vType == glpkInteger = return Integer
      getDomain' vType
        | vType == glpkBinary = return Binary
        | otherwise = throwIO unknownCode
        where
          typeName = T.pack . show . typeOf $ vType
          GlpkVariableType code = vType
          unknownCode = UnknownCode typeName code
   in do
        problem <- askProblem
        column <- readColumn variable
        getDomain' =<< liftIO (glp_get_col_kind problem column)

getVariableValue' :: GlpkVariable -> Glpk Double
getVariableValue' variable = do
  lastSolveRef <- asks _glpkLastSolveType
  lastSolve <- (liftIO . readIORef) lastSolveRef

  let method = case lastSolve of
        Nothing -> glp_get_col_prim
        Just LP -> glp_get_col_prim
        Just MIP -> glp_mip_col_val
        Just InteriorPoint -> glp_ipt_col_prim

  problem <- askProblem
  column <- readColumn variable
  liftIO $ realToFrac <$> method problem column

getTimeout' :: RealFrac a => Glpk a
getTimeout' =
  let fromMillis :: RealFrac a => CInt -> a
      fromMillis millis = realToFrac millis / 1000
   in do
        controlRef <- asks _glpkSimplexControl
        control <- liftIO (readIORef controlRef)
        return $ fromMillis (smcpTimeLimitMillis control)

setTimeout' :: RealFrac a => a -> Glpk ()
setTimeout' seconds =
  let millis :: Integer
      millis = round (seconds * 1000)
   in do
        controlRef <- asks _glpkSimplexControl
        control <- liftIO (readIORef controlRef)
        let control' = control {smcpTimeLimitMillis = fromIntegral millis}
        liftIO (writeIORef controlRef control')

setRelativeMIPGap' :: RealFrac a => a -> Glpk ()
setRelativeMIPGap' gap = do
  controlRef <- asks _glpkMIPControl
  control <- liftIO (readIORef controlRef)
  let control' = control {iocpRelativeMIPGap = realToFrac gap}
  liftIO (writeIORef controlRef control')

getRelativeMIPGap' :: RealFrac a => Glpk a
getRelativeMIPGap' = do
  controlRef <- asks _glpkMIPControl
  control <- liftIO (readIORef controlRef)
  return $ realToFrac (iocpRelativeMIPGap control)

solutionStatus :: GlpkSolutionStatus -> SolutionStatus
solutionStatus status
  | status == glpkOptimal = Optimal
  | status == glpkFeasible = Feasible
  | status == glpkInfeasible = Infeasible
  | status == glpkNoFeasible = Infeasible
  | status == glpkUnbounded = Unbounded
  | status == glpkUndefined = Infeasible
  | otherwise = Error

-- | Write out the current formulation to a file.
writeFormulation :: FilePath -> Glpk ()
writeFormulation fileName = do
  problem <- askProblem
  _ <- liftIO $ withCString fileName (glp_write_lp problem nullPtr)
  return ()

maxCDouble :: CDouble
maxCDouble = encodeFloat significand' exponent'
  where
    base = floatRadix (undefined :: CDouble)
    precision = floatDigits (undefined :: CDouble)
    (_, maxExponent) = floatRange (undefined :: CDouble)
    significand' = base ^ precision - 1
    exponent' = maxExponent - precision

withCText :: T.Text -> (CString -> IO a) -> IO a
withCText = withCString . T.unpack
