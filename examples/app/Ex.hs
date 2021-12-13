{-# LANGUAGE DerivingVia #-}
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
      LPMonad GlpkVariable GlpkConstraint GlpkObjective
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
  pure ()

--  case result of
--    Nothing -> putStrLn "Unsatisfiable"
--    Just vars -> do
--      _ <- flip M.traverseWithKey vars $ \(k, v) -> printf "%s=%s" k v
--      pure ()

program ::
  (MonadIO m, IPMonad v c o m, MonadState (M.Map Var v) m) =>
  Problem ->
  m (Maybe (M.Map Var Double))
program clauses = do
  forM_ clauses $ \(x, y, z) -> do
    vx <- getVar (fst x)
    vx @>=# 1
  -- ((termExpr x .+. termExpr y .+. termExpr z) :: _) .>=# 1
  status <- optimizeIP
  case status of
    Error -> liftIO (print "Error") >> pure Nothing
    Infeasible -> pure Nothing
    Optimal -> do
      vars <- get
      fmap pure (mapM getVariableValue vars)

termExpr :: (IPMonad v c o m, MonadState (M.Map T.Text v) m) => Term -> m (Expr v)
termExpr (v, b) = do
  x <- getVar v
  pure $
    if b
      then var x
      else 1 #-@ x

getVar :: (IPMonad v c o m, MonadState (M.Map T.Text v) m) => Var -> m v
getVar v = do
  vars <- get
  case M.lookup v vars of
    Nothing -> binary
    Just x -> pure x
