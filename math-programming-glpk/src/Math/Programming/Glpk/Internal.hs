{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the full definitions backing the simplified API
-- exposed in 'Math.Programming.Glpk'.
module Math.Programming.Glpk.Internal where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor
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
import UnliftIO
import UnliftIO.Concurrent

-- | A reference to a GLPK variable.
type GlpkVariable = GlpkPtr Column

-- | A reference to a GLPK constraint.
type GlpkConstraint = GlpkPtr Row

-- | A placeholder for an objective.
--
-- GLPK supports only single-objective problems, and so no indices
-- need to be stored.
newtype GlpkObjective = GlpkObjective ()

class (MonadLP GlpkVariable GlpkConstraint GlpkObjective m, MonadIP GlpkVariable GlpkConstraint GlpkObjective m) => MonadGlpk m where
  writeFormulation :: FilePath -> m ()

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

-- | A pointer to a GLPK row or column.
--
-- We assign an immutable unique value to each 'GlpkPtr' we create.
--
-- Internally, GLPK refers to variables and constraints by their
-- column and row indices, respectively. These indices can change when
-- rows and columns are deleted, so we update this value as necessary.
data GlpkPtr a = GlpkPtr
  { -- | An immutable, unique value associated with this pointer.
    _glpkPtrId :: Integer,
    -- | The referenced object.
    _glpkPtrRef :: IORef a,
    -- | Whether this reference has been deleted from the problem.
    _glpkPtrDeleted :: IORef Bool
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
newtype GlpkT m a = GlpkT {_runGlpk :: ReaderT GlpkEnv m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadTrans
    )

type Glpk = GlpkT IO

instance MonadLP GlpkVariable GlpkConstraint GlpkObjective Glpk where
  addVariable = addVariable'
  deleteVariable = deleteVariable'
  getVariableName = getVariableName'
  setVariableName = setVariableName'
  getVariableValue = getVariableValue'
  getVariableBounds = getVariableBounds'
  setVariableBounds = setVariableBounds'

  addConstraint = addConstraint'
  deleteConstraint = deleteConstraint'
  getConstraintName = getConstraintName'
  setConstraintName = setConstraintName'
  getConstraintValue = getDualValue

  addObjective = addObjective'
  deleteObjective = deleteObjective'
  getObjectiveName = getObjectiveName'
  setObjectiveName = setObjectiveName'
  getObjectiveValue = getObjectiveValue'
  getObjectiveSense = getSense'
  setObjectiveSense = setSense'

  getTimeout = getTimeout'
  setTimeout = setTimeout'
  optimizeLP = optimizeLP'

instance MonadIP GlpkVariable GlpkConstraint GlpkObjective Glpk where
  getVariableDomain = getVariableDomain'
  setVariableDomain = setVariableDomain'
  getRelativeMIPGap = getRelativeMIPGap'
  setRelativeMIPGap = setRelativeMIPGap'
  optimizeIP = optimizeIP'

instance MonadGlpk Glpk where
  writeFormulation = writeFormulation'

instance MonadGlpk m => MonadGlpk (ReaderT r m) where
  writeFormulation = lift . writeFormulation

instance MonadGlpk m => MonadGlpk (StateT s m) where
  writeFormulation = lift . writeFormulation

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

-- | Retrieve a component of the Glpk context
askGlpk :: Monad m => (GlpkEnv -> a) -> GlpkT m a
askGlpk = flip fmap (GlpkT ask)

-- | The underlying Glpk problem pointer
askProblem :: Monad m => GlpkT m (Ptr Problem)
askProblem = askGlpk _glpkEnvProblem

-- | All registered variables
askVariablesRef :: Glpk (IORef [GlpkVariable])
askVariablesRef = askGlpk _glpkVariables

-- | All registered constraints
askConstraintsRef :: Glpk (IORef [GlpkConstraint])
askConstraintsRef = askGlpk _glpkConstraints

-- | Note that a new row or column has been added to the to the problem.
register :: GlpkPtr a -> IORef [GlpkPtr a] -> Glpk ()
register newPtr ptrRefs = do
  liftIO $ modifyIORef' ptrRefs (newPtr :)

-- | Note that a row or column has been deleted from the problem, and
-- update row or column indices accordingly.
unregister :: Integral a => GlpkPtr a -> IORef [GlpkPtr a] -> Glpk ()
unregister deletedPtr ptrsRef =
  let update removed (GlpkPtr _ ptr _) = do
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
      <$> newIORef column
      <*> newIORef False

  askVariablesRef >>= register variable
  setVariableName' variable (defaultVariableName variable)
  pure variable

defaultVariableName :: GlpkVariable -> T.Text
defaultVariableName (GlpkPtr x _ _) = "x" <> T.pack (show x)

defaultConstraintName :: GlpkConstraint -> T.Text
defaultConstraintName (GlpkPtr x _ _) = "c" <> T.pack (show x)

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
  askVariablesRef >>= unregister variable

addConstraint' :: Inequality (Expr GlpkVariable) -> Glpk GlpkConstraint
addConstraint' (Inequality ordering lhs rhs) =
  let LinExpr terms constant = simplify (lhs .-. rhs) :: Expr GlpkVariable

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
            <$> newIORef row
            <*> newIORef False

        askConstraintsRef >>= register constraintPtr
        setConstraintName' constraintPtr (defaultConstraintName constraintPtr)
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
  askConstraintsRef >>= unregister constraint

addObjective' :: Expr GlpkVariable -> Glpk GlpkObjective
addObjective' expr =
  let LinExpr terms constant = simplify expr
   in do
        problem <- askProblem

        -- Set the constant term
        liftIO $ glp_set_obj_coef problem (GlpkInt 0) (realToFrac constant)

        -- Set the variable terms
        forM_ terms $ \(coef, variable) -> do
          column <- readColumn variable
          liftIO $ glp_set_obj_coef problem column (realToFrac coef)

        pure (GlpkObjective ())

-- | Delete an objective
--
-- There is nothing to actually delete, so we just set a zero objective
deleteObjective' :: GlpkObjective -> Glpk ()
deleteObjective' _ = void (addObjective' mempty)

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
  lastSolveRef <- askGlpk _glpkLastSolveType
  lastSolve <- (liftIO . readIORef) lastSolveRef
  fmap realToFrac . liftIO $ case lastSolve of
    Just MIP -> glp_mip_obj_val problem
    Just LP -> glp_get_obj_val problem
    Just InteriorPoint -> glp_ipt_obj_val problem
    Nothing -> glp_get_obj_val problem -- There's been no solve, so who cares

optimizeLP' :: Glpk SolutionStatus
optimizeLP' = do
  -- Note that we've run an LP solve
  solveTypeRef <- askGlpk _glpkLastSolveType
  liftIO $ writeIORef solveTypeRef (Just LP)

  -- Run Simplex
  problem <- askProblem
  controlRef <- askGlpk _glpkSimplexControl
  liftIO $ do
    control <- readIORef controlRef
    alloca $ \controlPtr -> do
      poke controlPtr control
      _ <- glp_simplex problem controlPtr
      glp_get_status problem Data.Functor.<&> solutionStatus

optimizeIP' :: Glpk SolutionStatus
optimizeIP' = do
  -- Note that we've run a MIP solve
  solveTypeRef <- askGlpk _glpkLastSolveType
  liftIO $ writeIORef solveTypeRef (Just MIP)

  problem <- askProblem
  controlRef <- askGlpk _glpkMIPControl
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
  lastSolveRef <- askGlpk _glpkLastSolveType
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
        controlRef <- askGlpk _glpkSimplexControl
        control <- liftIO (readIORef controlRef)
        return $ fromMillis (smcpTimeLimitMillis control)

setTimeout' :: RealFrac a => a -> Glpk ()
setTimeout' seconds =
  let millis :: Integer
      millis = round (seconds * 1000)
   in do
        controlRef <- askGlpk _glpkSimplexControl
        control <- liftIO (readIORef controlRef)
        let control' = control {smcpTimeLimitMillis = fromIntegral millis}
        liftIO (writeIORef controlRef control')

setRelativeMIPGap' :: RealFrac a => a -> Glpk ()
setRelativeMIPGap' gap = do
  controlRef <- askGlpk _glpkMIPControl
  control <- liftIO (readIORef controlRef)
  let control' = control {iocpRelativeMIPGap = realToFrac gap}
  liftIO (writeIORef controlRef control')

getRelativeMIPGap' :: RealFrac a => Glpk a
getRelativeMIPGap' = do
  controlRef <- askGlpk _glpkMIPControl
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
writeFormulation' :: FilePath -> Glpk ()
writeFormulation' fileName = do
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
