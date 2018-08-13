{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Programming.Glpk where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr

import Math.Programming
import Math.Programming.Glpk.Header

newtype Glpk a = Glpk { runGlpk :: ReaderT (Ptr Problem) IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Ptr Problem)
    )

toCDouble :: Double -> CDouble
toCDouble = realToFrac

toCInt :: Int -> CInt
toCInt = fromIntegral

instance LPMonad Glpk Double where
  makeVariable = do
    problem <- ask
    Column col <- liftIO $ glp_add_cols problem 1
    return (Variable (fromIntegral col))

  addConstraint (Constraint (LinearExpr terms constant) ordering) =
    let
      rhs :: CDouble
      rhs = toCDouble (negate constant)

      numVars :: CInt
      numVars = toCInt (length terms)

      getVar :: (Variable, Double) -> Column
      getVar ((Variable v), _) = Column (fromIntegral v)

      glpkOrdering LT = glpkLT
      glpkOrdering GT = glpkGT
      glpkOrdering EQ = glpkBounded
    in do
      problem <- ask
      row <- liftIO $ glp_add_rows problem 1
      liftIO $ do
        varIndices <- mkGlpkArray (fmap getVar terms)
        varCoefs <- mkGlpkArray (fmap (toCDouble . snd) terms)
        glp_set_row_bnds problem row (glpkOrdering ordering) rhs rhs
        glp_set_mat_row problem row numVars varIndices varCoefs
        free (fromGplkArray varIndices)
        free (fromGplkArray varCoefs)

  setObjective (LinearExpr terms constant) = do
    problem <- ask

    -- Set the constant term
    liftIO $ glp_set_obj_coef problem (Column 0) (toCDouble constant)

    -- Set the variable terms
    liftIO $ forM_ terms $ \(Variable column, coef) ->
      glp_set_obj_coef problem (Column (toCInt column)) (toCDouble coef)

  setSense sense =
    let
      direction = case sense of
        Minimization -> glpkMin
        Maximization -> glpkMax
    in do
      problem <- ask
      liftIO $ glp_set_obj_dir problem direction

  optimize =
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
      problem <- ask
      result <- liftIO $ glp_simplex problem nullPtr
      liftIO $ convertResult problem result

  setVariableBounds (Variable variable) bounds =
    let
      column = Column (toCInt variable)

      (boundType, low, high) = case bounds of
        Free -> (glpkFree, 0, 0)
        NonNegativeReals -> (glpkGT, 0, 0)
        NonPositiveReals -> (glpkLT, 0, 0)
        Interval low high -> (glpkBounded, low, high)

    in do
      problem <- ask
      liftIO $ glp_set_col_bnds problem column boundType (toCDouble low) (toCDouble high)

  setVariableDomain (Variable variable) domain =
    let
      column = Column (toCInt variable)
      vType = case domain of
        Continuous -> glpkContinuous
        Integer -> glpkInteger
        Binary -> glpkBinary
    in do
      problem <- ask
      liftIO $ glp_set_col_kind problem column vType

  evaluateVariable (Variable variable) =
    let
      column = Column (toCInt variable)
    in do
      problem <- ask
      liftIO $ realToFrac <$> glp_get_col_prim problem column

  evaluateExpression (LinearExpr terms constant) =
    let
      variables = fmap fst terms
      coefs = fmap snd terms
    in do
      values <- mapM evaluate variables
      return $ constant + sum (zipWith (*) values coefs)
