{-# LANGUAGE OverloadedStrings #-}

module Optimizer
  ( checkConstExpr
  , optConstExpr
  , algebraicOpt
  ) where

import qualified Data.Map as M
import Data.Text (Text)

import Expression

-- | Returns True if the expression can be evaluated at compile time
checkConstExpr :: Expr -> Bool
checkConstExpr (IntLit _) = True
checkConstExpr (VarLit _) = False
checkConstExpr (Sum a b) = checkConstExpr a && checkConstExpr b
checkConstExpr (Sub a b) = checkConstExpr a && checkConstExpr b
checkConstExpr (Prod a b) = checkConstExpr a && checkConstExpr b
checkConstExpr (Div a b) = checkConstExpr a && checkConstExpr b
checkConstExpr (Neg a) = checkConstExpr a

-- | Optimizes a constant expression by evaluating it at compile time
optConstExpr :: Expr -> Expr
optConstExpr expr
  | checkConstExpr expr = IntLit (evalExpr expr M.empty)
  | otherwise = case expr of
      IntLit n -> IntLit n
      VarLit v -> VarLit v
      Sum a b -> Sum (optConstExpr a) (optConstExpr b)
      Sub a b -> Sub (optConstExpr a) (optConstExpr b)
      Prod a b -> Prod (optConstExpr a) (optConstExpr b)
      Div a b -> Div (optConstExpr a) (optConstExpr b)
      Neg a -> Neg (optConstExpr a)

-- | Applies algebraic optimizations
algebraicOpt :: Expr -> Expr
algebraicOpt expr = case expr of
  -- Handle basic expressions
  IntLit n -> IntLit n
  VarLit v -> VarLit v

  Sum (IntLit 0) b -> algebraicOpt b         -- 0 + b = b
  Sum a (IntLit 0) -> algebraicOpt a         -- a + 0 = a
  Sum a b -> Sum (algebraicOpt a) (algebraicOpt b)
  
  Sub a (IntLit 0) -> algebraicOpt a         -- a - 0 = a
  Sub (IntLit 0) b -> Neg (algebraicOpt b)   -- 0 - b = -b
  Sub a b -> Sub (algebraicOpt a) (algebraicOpt b)
  
  Prod (IntLit 0) _ -> IntLit 0              -- 0 * b = 0
  Prod _ (IntLit 0) -> IntLit 0              -- a * 0 = 0
  Prod (IntLit 1) b -> algebraicOpt b        -- 1 * b = b
  Prod a (IntLit 1) -> algebraicOpt a        -- a * 1 = a
  Prod a b -> Prod (algebraicOpt a) (algebraicOpt b)
  
  Div (IntLit 0) _ -> IntLit 0               -- 0 / b = 0 (assuming b != 0)
  Div a (IntLit 1) -> algebraicOpt a         -- a / 1 = a
  Div a b -> Div (algebraicOpt a) (algebraicOpt b)
  
  Neg (Neg a) -> algebraicOpt a              -- -(-a) = a
  Neg (IntLit 0) -> IntLit 0                 -- -0 = 0
  Neg a -> Neg (algebraicOpt a)

