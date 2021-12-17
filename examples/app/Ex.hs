{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Math.Programming
import Math.Programming.Glpk
import Text.Printf

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
      LPMonad GlpkVariable GlpkConstraint GlpkObjective,
      IPMonad GlpkVariable GlpkConstraint GlpkObjective
    )

runAppM :: AppM a -> IO a
runAppM = runGlpk . flip evalStateT M.empty . unAppM

main :: IO ()
main = do
  eProblem <- parseProblem <$> TIO.getContents
  case eProblem of
    Left err -> TIO.putStrLn err
    Right problem -> solve problem

type Var = T.Text

type Term = (T.Text, Bool)

type Problem = [(Term, Term, Term)]

parseProblem :: T.Text -> Either T.Text Problem
parseProblem input = mapM parseClause (T.lines input)
  where
    parseClause :: T.Text -> Either T.Text (Term, Term, Term)
    parseClause clause = case T.words clause of
      [x, y, z] -> (,,) <$> term x <*> term y <*> term z
      _ -> Left clause

    term :: T.Text -> Either T.Text Term
    term var = case T.stripPrefix "-" var of
      Nothing -> Right (var, True)
      Just v -> if T.null v then Left var else Right (v, False)

solve :: Problem -> IO ()
solve problem = do
  result <- runAppM (program problem)
  case result of
    Nothing -> putStrLn "Unsatisfiable"
    Just vars -> do
      putStrLn "Satisfiable"
      _ <- flip M.traverseWithKey vars $ \k v -> printf "%s=%f\n" k v
      pure ()

program ::
  (MonadIO m, IPMonad v c o m, MonadState (M.Map Var v) m) =>
  Problem ->
  m (Maybe (M.Map Var Double))
program clauses = do
  forM_ clauses $ \(x, y, z) -> do
    vx <- termExpr x
    vy <- termExpr y
    vz <- termExpr z
    vx .+ vy .+ vz .>=# 1
  status <- optimizeIP
  case status of
    Error -> liftIO (print "Error") >> pure Nothing
    Infeasible -> pure Nothing
    Optimal -> do
      vars <- get
      forM_ vars (getName >=> (liftIO . print))
      fmap pure (mapM getVariableValue vars)

termExpr :: (IPMonad v c o m, MonadState (M.Map Var v) m) => Term -> m (Expr v)
termExpr (v, b) = do
  x <- getVar v
  pure $
    if b
      then 1 .* x
      else (-1) .* x

getVar :: (IPMonad v c o m, MonadState (M.Map Var v) m) => Var -> m v
getVar v = do
  vars <- get
  case M.lookup v vars of
    Just x -> pure x
    Nothing -> do
      x <- binary
      put (M.insert v x vars)
      pure x
