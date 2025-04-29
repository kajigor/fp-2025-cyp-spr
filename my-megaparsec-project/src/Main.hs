{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T (Text, pack)
import Text.Megaparsec
import qualified Data.Map as M
import Control.Monad.State
import System.IO
import System.Environment
import qualified Control.Exception as E (try, evaluate, SomeException)

import Expression
import Statement
import Optimizer

-- | Helper function to safely evaluate expressions that might throw exceptions
safeEval :: Expr -> Subst -> IO (Either String Int)
safeEval expr subst = do
  result <- E.try (E.evaluate (evalExpr expr subst)) :: IO (Either E.SomeException Int)
  case result of
    Left ex -> return $ Left $ "Error: " ++ show ex
    Right val -> return $ Right val

-- | Demonstrate optimization of a specific expression
demonstrateOptimization :: Expr -> Subst -> IO ()
demonstrateOptimization expr subst = do
  putStrLn "Original Expression:"
  putStrLn $ prettyPrintExpr expr
  
  putStrLn "\nVariable Values:"
  mapM_ (\(var, val) -> putStrLn $ show var ++ " = " ++ show val) (M.toList subst)
  
  -- Show the algebraic optimization
  let algebraicOptimized = algebraicOpt expr
  putStrLn "\nAfter Algebraic Optimization:"
  putStrLn $ prettyPrintExpr algebraicOptimized
  
  -- Show the constant expression optimization
  let constOptimized = optConstExpr expr
  putStrLn "\nAfter Constant Expression Optimization:"
  putStrLn $ prettyPrintExpr constOptimized
  
  -- Show combined optimization
  let fullOptimized = algebraicOpt (optConstExpr expr)
  putStrLn "\nAfter Combined Optimization:"
  putStrLn $ prettyPrintExpr fullOptimized
  
  -- Try to evaluate the expressions
  putStrLn "\nEvaluation Results:"
  
  putStr "Original: "
  evalResult <- safeEval expr subst
  putStrLn $ case evalResult of
    Left err -> err
    Right val -> show val
  
  putStr "Algebraic Optimized: "
  algebraicResult <- safeEval algebraicOptimized subst
  putStrLn $ case algebraicResult of
    Left err -> err
    Right val -> show val
  
  putStr "Const Optimized: "
  constResult <- safeEval constOptimized subst
  putStrLn $ case constResult of
    Left err -> err
    Right val -> show val
  
  putStr "Full Optimized: "
  fullResult <- safeEval fullOptimized subst
  putStrLn $ case fullResult of
    Left err -> err
    Right val -> show val

main :: IO ()
main = do
  putStrLn "Expression Optimization Demo"
  putStrLn "----------------------------\n"
  
  -- The expression from the failing test: Div (IntLit 0) (Div (Neg (VarLit "e")) (VarLit "y"))
  -- with variables e = 9 and y = -32
  let failingExpr = Div (IntLit 0) (Div (Neg (VarLit "e")) (VarLit "y"))
  let varSubst = M.fromList [("e", 9), ("y", -32)]
  
  demonstrateOptimization failingExpr varSubst
  
  putStrLn "\n=============================================\n"
  
  -- The complex failing test case from property test
  putStrLn "Testing complex division expression that failed in property tests:"
  let complexFailingExpr = Div (IntLit 48) (Sub (Div (Sum (IntLit 96) (IntLit (-96))) (Div (IntLit (-5)) (VarLit "g"))) (IntLit 100))
  let complexSubst = M.fromList [("g", -82)]
  
--  demonstrateOptimization complexFailingExpr complexSubst

  putStrLn "Another complex division expression that failed in property tests:"
  let complexFailingExpr =  Prod (Div (IntLit 27) (Div (Neg (IntLit (-23))) (Prod (VarLit "i") (IntLit 88)))) (Prod (Div (Neg (Div (IntLit 76) (IntLit (-99)))) (IntLit 20)) (VarLit "c"))
  let complexSubst = M.fromList [("c",-67),("i",8)]

--  demonstrateOptimization complexFailingExpr complexSubst

  putStrLn "Another complex division expression that failed in property tests:"
  let complexFailingExpr =   Sum (Div (IntLit (-65)) (Div (IntLit 63) (IntLit 96))) (VarLit "j")
  let complexSubst = M.fromList []

  demonstrateOptimization complexFailingExpr complexSubst
  
  putStrLn "\nDone!"



