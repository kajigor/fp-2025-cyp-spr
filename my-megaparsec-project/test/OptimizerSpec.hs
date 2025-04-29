{-# LANGUAGE OverloadedStrings #-}

module OptimizerSpec (spec) where

import Test.Hspec
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

import Expression
import Statement
import Optimizer

spec :: Spec
spec = do
  describe "Constant Expression Optimization" $ do
    it "identifies constant expressions correctly" $ do
      checkConstExpr (IntLit 5) `shouldBe` True
      checkConstExpr (VarLit "x") `shouldBe` False
      checkConstExpr (Sum (IntLit 3) (IntLit 4)) `shouldBe` True
      checkConstExpr (Sum (IntLit 3) (VarLit "x")) `shouldBe` False
      checkConstExpr (Neg (IntLit 5)) `shouldBe` True
      checkConstExpr (Neg (VarLit "x")) `shouldBe` False
      checkConstExpr (Prod (IntLit 2) (Sum (IntLit 3) (IntLit 4))) `shouldBe` True
      
    it "evaluates constant expressions at compile time" $ do
      optConstExpr (IntLit 5) `shouldBe` IntLit 5
      optConstExpr (VarLit "x") `shouldBe` VarLit "x"
      optConstExpr (Sum (IntLit 3) (IntLit 4)) `shouldBe` IntLit 7
      optConstExpr (Prod (IntLit 2) (Sum (IntLit 3) (IntLit 4))) `shouldBe` IntLit 14
      optConstExpr (Neg (Sum (IntLit 3) (IntLit 4))) `shouldBe` IntLit (-7)
      
    it "partially optimizes expressions with variables" $ do
      let expr = Sum (VarLit "x") (Sum (IntLit 3) (IntLit 4))
      optConstExpr expr `shouldBe` Sum (VarLit "x") (IntLit 7)
      
      let expr2 = Prod (Sum (IntLit 2) (IntLit 3)) (VarLit "x")
      optConstExpr expr2 `shouldBe` Prod (IntLit 5) (VarLit "x")
      
  describe "Algebraic Optimizations" $ do
    it "optimizes addition with identity element" $ do
      algebraicOpt (Sum (IntLit 0) (VarLit "x")) `shouldBe` VarLit "x"
      algebraicOpt (Sum (VarLit "x") (IntLit 0)) `shouldBe` VarLit "x"
      
    it "optimizes subtraction with identity element" $ do
      algebraicOpt (Sub (VarLit "x") (IntLit 0)) `shouldBe` VarLit "x"
      algebraicOpt (Sub (IntLit 0) (VarLit "x")) `shouldBe` Neg (VarLit "x")
      
    it "optimizes multiplication with identity and zero" $ do
      algebraicOpt (Prod (IntLit 1) (VarLit "x")) `shouldBe` VarLit "x"
      algebraicOpt (Prod (VarLit "x") (IntLit 1)) `shouldBe` VarLit "x"
      algebraicOpt (Prod (IntLit 0) (VarLit "x")) `shouldBe` IntLit 0
      algebraicOpt (Prod (VarLit "x") (IntLit 0)) `shouldBe` IntLit 0
      
    it "optimizes division with identity" $ do
      algebraicOpt (Div (VarLit "x") (IntLit 1)) `shouldBe` VarLit "x"
      algebraicOpt (Div (IntLit 0) (VarLit "x")) `shouldBe` IntLit 0
      
    it "optimizes negation" $ do
      algebraicOpt (Neg (Neg (VarLit "x"))) `shouldBe` VarLit "x"
      algebraicOpt (Neg (IntLit 0)) `shouldBe` IntLit 0
      
    it "performs nested optimizations" $ do
      let expr = Sum (Prod (IntLit 0) (VarLit "x")) (Sum (VarLit "y") (IntLit 0))
      algebraicOpt expr `shouldBe` VarLit "y"
      
      let expr2 = Prod (Sum (IntLit 0) (VarLit "x")) (Div (VarLit "y") (IntLit 1))
      algebraicOpt expr2 `shouldBe` Prod (VarLit "x") (VarLit "y")
      
  describe "Statement Optimization" $ do
    it "optimizes assignment statements with constant expressions" $ do
      let stmt = Assign "x" (Sum (IntLit 3) (IntLit 4))
      let (optimized, subst) = optStmt stmt M.empty
      optimized `shouldBe` Assign "x" (IntLit 7)
      M.lookup "x" subst `shouldBe` Just 7
      
    it "optimizes assignments with variables using substitution" $ do
      let stmt = Assign "y" (Sum (VarLit "x") (IntLit 5))
      let subst = M.fromList [("x", 10)]
      let (optimized, subst') = optStmt stmt subst
      optimized `shouldBe` Assign "y" (IntLit 15)
      M.lookup "y" subst' `shouldBe` Just 15
      
    it "optimizes write statements" $ do
      let stmt = Write (Sum (IntLit 3) (IntLit 4))
      let (optimized, _) = optStmt stmt M.empty
      optimized `shouldBe` Write (IntLit 7)
      
    it "optimizes sequence of statements" $ do
      let stmt = Seq 
                  (Assign "x" (IntLit 5)) 
                  (Assign "y" (Sum (VarLit "x") (IntLit 3)))
      let (optimized, subst) = optStmt stmt M.empty
      case optimized of
        Seq (Assign x1 e1) (Assign y1 e2) -> do
          x1 `shouldBe` "x"
          e1 `shouldBe` IntLit 5
          y1 `shouldBe` "y"
          e2 `shouldBe` IntLit 8
        _ -> expectationFailure "Expected a sequence of two assignments"
      M.lookup "x" subst `shouldBe` Just 5
      M.lookup "y" subst `shouldBe` Just 8
      
    it "optimizes while conditions" $ do
      let stmt = While (Sum (IntLit 1) (IntLit 2)) (Assign "x" (IntLit 5))
      let (While cond' body', _) = optStmt stmt M.empty
      cond' `shouldBe` IntLit 3
      body' `shouldBe` Assign "x" (IntLit 5)
      
    it "preserves semantics during optimization" $ do
      let stmt = Seq
                  (Assign "x" (IntLit 10))
                  (Seq
                    (Assign "y" (Sum (VarLit "x") (IntLit 5)))
                    (Write (Sum (VarLit "x") (VarLit "y"))))
      let (optimized, _) = optStmt stmt M.empty
      case optimized of
        Seq (Assign x1 e1) (Seq (Assign y1 e2) (Write e3)) -> do
          x1 `shouldBe` "x"
          e1 `shouldBe` IntLit 10
          y1 `shouldBe` "y"
          e2 `shouldBe` IntLit 15
          e3 `shouldBe` IntLit 25
        _ -> expectationFailure "Expected a nested sequence of statements"
        
  describe "Variable Tracking" $ do
    it "identifies modified variables in statements" $ do
      modifiedVars (Assign "x" (IntLit 5)) `shouldBe` S.singleton "x"
      modifiedVars (Read "y") `shouldBe` S.singleton "y"
      modifiedVars (Write (VarLit "z")) `shouldBe` S.empty
      modifiedVars (Seq (Assign "a" (IntLit 1)) (Assign "b" (IntLit 2))) 
        `shouldBe` S.fromList ["a", "b"]
      modifiedVars (While (VarLit "c") (Assign "d" (IntLit 3)))
        `shouldBe` S.singleton "d"
        
    it "can identify when statements can be evaluated statically" $ do
      let subst = M.fromList [("x", 5), ("y", 10)]
      canEvalStmt (Assign "z" (Sum (VarLit "x") (VarLit "y"))) subst `shouldBe` True
      canEvalStmt (Assign "z" (Sum (VarLit "x") (VarLit "unknown"))) subst `shouldBe` False
      canEvalStmt (Write (Sum (IntLit 1) (IntLit 2))) subst `shouldBe` True
      canEvalStmt (Read "z") subst `shouldBe` False
