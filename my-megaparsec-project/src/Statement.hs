{-# LANGUAGE OverloadedStrings #-}

module Statement
  ( Stmt(..)
  , evalStmt
  , prettyPrintStmt
  ) where

import Data.Text (Text)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B
import Data.Void
import Text.Megaparsec.Char
import Control.Monad (void)

import Parser
import Expression (Expr, Subst, evalExpr, prettyPrintExpr, parseExpr)

data Stmt
  = Assign Text Expr
  | Seq Stmt Stmt
  | While Expr Stmt
  | Write Expr
  | Read Text
  deriving (Show, Eq)

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

prettyPrintStmt :: Stmt -> String
prettyPrintStmt (Assign var e) = show var ++ " = " ++ prettyPrintExpr e ++ ";"
prettyPrintStmt (Seq s1 s2) = prettyPrintStmt s1 ++ "\n" ++ prettyPrintStmt s2
prettyPrintStmt (While cond body) = "while (" ++ prettyPrintExpr cond ++ ") {\n" ++ prettyPrintStmt body ++ "\n};"
prettyPrintStmt (Write expr) = "print(" ++ prettyPrintExpr expr ++ ");"
prettyPrintStmt (Read var) = show var ++ " = readLine();"

-- TODO:
-- parseStmt
-- parseAssign
-- parseWhile
-- parseWrite
-- parseRead
-- parseBlock