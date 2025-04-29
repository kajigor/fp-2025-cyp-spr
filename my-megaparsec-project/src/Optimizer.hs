{-# LANGUAGE OverloadedStrings #-}

module Optimizer
  ( checkConstExpr
  , optConstExpr
  , algebraicOpt
  , canEvalStmt
  , optStmt
  , modifiedVars
  , optimizeWithEmptySubst
  ) where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Set as S
import Control.Monad.State

import Expression
import Statement

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

-- | Collects all variables used in an expression
getVarsInExpr :: Expr -> S.Set Text
getVarsInExpr (IntLit _) = S.empty
getVarsInExpr (VarLit v) = S.singleton v
getVarsInExpr (Sum a b) = S.union (getVarsInExpr a) (getVarsInExpr b)
getVarsInExpr (Sub a b) = S.union (getVarsInExpr a) (getVarsInExpr b)
getVarsInExpr (Prod a b) = S.union (getVarsInExpr a) (getVarsInExpr b)
getVarsInExpr (Div a b) = S.union (getVarsInExpr a) (getVarsInExpr b)
getVarsInExpr (Neg a) = getVarsInExpr a

-- | Checks if an expression can be evaluated with the given substitution
canEvalExpr :: Expr -> Subst -> Bool
canEvalExpr expr subst = 
  all (`M.member` subst) (S.toList $ getVarsInExpr expr)

-- | Collects all variables that are modified (assigned to) in a statement
modifiedVars :: Stmt -> S.Set Text
modifiedVars (Assign var _) = S.singleton var
modifiedVars (Seq s1 s2) = S.union (modifiedVars s1) (modifiedVars s2)
modifiedVars (While _ body) = modifiedVars body
modifiedVars (Write _) = S.empty
modifiedVars (Read var) = S.singleton var


-- | Computes the resulting substitution after evaluating a statement
evalStaticStmt :: Stmt -> Subst -> Subst
evalStaticStmt (Assign var expr) subst = 
  if canEvalExpr expr subst
  then M.insert var (evalExpr expr subst) subst
  else M.delete var subst
evalStaticStmt (Seq s1 s2) subst =
  let subst' = evalStaticStmt s1 subst
  in evalStaticStmt s2 subst'
-- For a while loop, any variable modified in the body becomes unknown
evalStaticStmt (While _ body) subst = foldr M.delete subst (S.toList $ modifiedVars body)
evalStaticStmt (Write _) subst = subst
evalStaticStmt (Read var) subst = M.delete var subst

-- | Checks if a statement can be evaluated with the given substitution
canEvalStmt :: Stmt -> Subst -> Bool
canEvalStmt (Assign _ expr) subst = canEvalExpr expr subst
canEvalStmt (Seq s1 s2) subst =
  let canEval1 = canEvalStmt s1 subst
      -- If we can evaluate s1, use its result for s2's evaluation
      subst' = if canEval1 
               then evalStaticStmt s1 subst
               else foldr M.delete subst (S.toList $ modifiedVars s1)
  in canEval1 && canEvalStmt s2 subst'
canEvalStmt (While cond _) subst = False -- to complicated for now
canEvalStmt (Write expr) subst = canEvalExpr expr subst
canEvalStmt (Read _) _ = False

-- | Optimizes a statement by evaluating constant expressions
-- | Returns the optimized statement and the updated substitution map
optStmt :: Stmt -> Subst -> (Stmt, Subst)
optStmt (Read var) subst = (Read var, M.delete var subst)
optStmt (Assign var expr) subst =
  let optimizedExpr = algebraicOpt $ optConstExpr expr
      -- If expression is constant, evaluate it
      finalExpr = if canEvalExpr optimizedExpr subst
                  then IntLit (evalExpr optimizedExpr subst)
                  else optimizedExpr
      -- Update substitution if we've evaluated to a constant
      newSubst = case finalExpr of
                   IntLit n -> M.insert var n subst
                   _        -> M.delete var subst
  in (Assign var finalExpr, newSubst)

optStmt (Seq s1 s2) subst =
  let (s1', subst1) = optStmt s1 subst
      (s2', subst2) = optStmt s2 subst1
  in (Seq s1' s2', subst2)

optStmt (While cond body) subst =
  let optimizedCond = algebraicOpt $ optConstExpr cond
      -- Remove variables modified in body from substitution
      bodyModVars = modifiedVars body
      subst' = foldr M.delete subst bodyModVars
      -- We do not opt body, as cycle can do several iterations
  in (While optimizedCond body, subst')

optStmt (Write expr) subst =
  let optimizedExpr = algebraicOpt $ optConstExpr expr
      finalExpr = if canEvalExpr optimizedExpr subst
                  then IntLit (evalExpr optimizedExpr subst)
                  else optimizedExpr
  in (Write finalExpr, subst)

-- | Wrapper function that just returns the optimized statement
optimizeStatementWithSubst :: Stmt -> Subst -> Stmt
optimizeStatementWithSubst stmt subst = fst $ optStmt stmt subst

-- | Optimizes a statement using an empty substitution map
optimizeStmt :: Stmt -> Stmt
optimizeStmt stmt = optimizeStatement stmt M.empty
