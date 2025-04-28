{-# LANGUAGE OverloadedStrings #-}

module Statement
  ( Stmt(..)
  , evalStmt
  ) where

import Data.Text (Text)
import Expression (Expr, Subst, evalExpr)
import qualified Data.Map as M
import Control.Monad.State

data Stmt
  = Assign Text Expr
  | Seq Stmt Stmt
  | While Expr Stmt
  | Write Expr
  | Read Text

evalStmt :: Stmt -> State (Subst, [Int]) [Int]
evalStmt (Assign var e) = do
  (subst, input) <- get
  let e' = evalExpr e subst
  put (M.insert var e' subst, input)
  return []

evalStmt (Seq s1 s2) = do
  out1 <- evalStmt s1
  out2 <- evalStmt s2
  return (out1 ++ out2)

evalStmt (Write e) = do
  (subst, input) <- get
  return [evalExpr e subst]

evalStmt (Read var) = do
  (subst, input) <- get
  case input of
    []     -> error "No more input values!"
    (x:xs) -> do
      put (M.insert var x subst, xs)
      return []

evalStmt (While cond body) = do
  (subst, input) <- get
  if evalExpr cond subst == 0
    then return []
    else do
      put (subst, input)
      out1 <- evalStmt body
      out2 <- evalStmt (While cond body)
      return (out1 ++ out2)

