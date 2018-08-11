{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Math.Programming.Glpk where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List
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

instance LP Glpk where
  makeVariable = do
    problem <- ask
    Column col <- liftIO $ glp_add_cols problem 1
    return (Variable (fromIntegral col))

  makeConstraint = do
    problem <- ask
    Row row <- liftIO $ glp_add_rows problem 1
    return (Constraint (fromIntegral row))

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
