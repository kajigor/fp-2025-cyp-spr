{-# LANGUAGE OverloadedStrings #-}

module OptimizerPropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack)
import Control.Monad.State
import Data.List (nub)

import Expression
import Statement
import Optimizer

-- | Generate arbitrary variable names
genVarName :: Gen Text
genVarName = do
  c <- elements ['a'..'z']
  return $ pack [c]

-- | Generate arbitrary integer literals within a reasonable range
genIntLit :: Gen Int
genIntLit = choose (-100, 100)

-- Helper for checking if expression is zero
isZero :: Expr -> Bool
isZero (IntLit 0) = True
isZero _ = False

-- | Generate arbitrary expressions
instance Arbitrary Expr where
  arbitrary = sized genExpr
  
  -- Shrink implementation to simplify failing test cases
  shrink (Sum a b) = [a, b] ++ [Sum a' b | a' <- shrink a] ++ [Sum a b' | b' <- shrink b]
  shrink (Sub a b) = [a, b] ++ [Sub a' b | a' <- shrink a] ++ [Sub a b' | b' <- shrink b]
  shrink (Prod a b) = [a, b] ++ [Prod a' b | a' <- shrink a] ++ [Prod a b' | b' <- shrink b]
  shrink (Div a b) = [a] ++ [Div a' b | a' <- shrink a] ++ [Div a b' | b' <- shrink b, not $ isZero b']
  shrink (Neg a) = [a] ++ [Neg a' | a' <- shrink a]
  shrink _ = []

-- | Generate expressions with a size limit to avoid infinite recursion
genExpr :: Int -> Gen Expr
genExpr 0 = oneof [
    IntLit <$> genIntLit,
    VarLit <$> genVarName
  ]
genExpr n | n > 0 = oneof [
    IntLit <$> genIntLit,
    VarLit <$> genVarName,
    Sum <$> genExpr' <*> genExpr',
    Sub <$> genExpr' <*> genExpr',
    Prod <$> genExpr' <*> genExpr',
    Div <$> genExpr' <*> (suchThat (genExpr') (\e -> not $ isZero e)),
    Neg <$> genExpr'
  ]
  where
    genExpr' = genExpr (n `div` 2)

-- | Generate arbitrary statements
instance Arbitrary Stmt where
  arbitrary = sized genStmt
  
  -- Shrink implementation to simplify failing test cases
  shrink (Seq a b) = [a, b] ++ [Seq a' b | a' <- shrink a] ++ [Seq a b' | b' <- shrink b]
  shrink (While e s) = [s] ++ [While e s' | s' <- shrink s]
  shrink _ = []

-- | Generate statements with a size limit to avoid infinite recursion
genStmt :: Int -> Gen Stmt
genStmt 0 = oneof [
    Assign <$> genVarName <*> genExpr 0,
    Read <$> genVarName,
    Write <$> genExpr 0
  ]
genStmt n | n > 0 = frequency [
    (3, Assign <$> genVarName <*> genExpr (n `div` 2)),
    (1, Read <$> genVarName),
    (2, Write <$> genExpr (n `div` 2)),
    (3, Seq <$> genStmt (n `div` 2) <*> genStmt (n `div` 2)),
    (1, While <$> genExpr (n `div` 2) <*> genStmt (n `div` 2))
  ]

-- | Generate list of inputs for program execution
genInputs :: Gen [Int]
genInputs = listOf genIntLit

-- | Run a program with given inputs and return its output
runProgram :: Stmt -> [Int] -> [Int]
runProgram prog inputs = evalState (evalStmt prog) (M.empty, inputs)

-- | Property: Optimization preserves semantics
prop_optimizationPreservesSemantics :: Stmt -> [Int] -> Property
prop_optimizationPreservesSemantics prog inputs = 
  not (null inputs) ==> originalOutput == optimizedOutput
  where
    validInputs = take 10 $ inputs  -- Limit input size to avoid excessive computation
    originalOutput = runProgram prog validInputs
    optimizedProg = optimizeWithEmptySubst prog
    optimizedOutput = runProgram optimizedProg validInputs

-- | Property: Optimized expressions evaluate to the same value as the original
prop_expressionOptimizationCorrect :: Expr -> Property
prop_expressionOptimizationCorrect expr =
  forAll (genSubst (getVarsInExpr expr)) $ \subst ->
    canEvalExpr expr subst ==> 
      evalExpr expr subst == evalExpr (algebraicOpt expr) subst
  where
    genSubst vars = do
      let varList = S.toList vars
      values <- vectorOf (length varList) genIntLit
      return $ M.fromList (zip varList values)

-- | Property: Constant expression optimization is correct
prop_constExprOptimizationCorrect :: Expr -> Property
prop_constExprOptimizationCorrect expr =
  checkConstExpr expr ==>
    evalExpr expr M.empty == evalExpr (optConstExpr expr) M.empty

-- | Property: Optimization reduces program complexity
prop_optimizationReducesComplexity :: Stmt -> Property
prop_optimizationReducesComplexity prog =
  property $ (complexity optimizedProg) <= (complexity prog)
  where
    optimizedProg = optimizeWithEmptySubst prog
    
    -- Simple complexity metric: count number of nodes in the AST
    complexity :: Stmt -> Int
    complexity (Assign _ e) = 1 + exprComplexity e
    complexity (Seq s1 s2) = 1 + complexity s1 + complexity s2
    complexity (While e s) = 1 + exprComplexity e + complexity s
    complexity (Write e) = 1 + exprComplexity e
    complexity (Read _) = 1
    
    exprComplexity :: Expr -> Int
    exprComplexity (IntLit _) = 1
    exprComplexity (VarLit _) = 1
    exprComplexity (Sum a b) = 1 + exprComplexity a + exprComplexity b
    exprComplexity (Sub a b) = 1 + exprComplexity a + exprComplexity b
    exprComplexity (Prod a b) = 1 + exprComplexity a + exprComplexity b
    exprComplexity (Div a b) = 1 + exprComplexity a + exprComplexity b
    exprComplexity (Neg a) = 1 + exprComplexity a

-- | Property: Algebraic optimization always terminates
prop_algebraicOptTerminates :: Expr -> Bool
prop_algebraicOptTerminates expr = 
  algebraicOpt expr `seq` True

-- | Run tests with specific examples that demonstrate optimization benefits
spec :: Spec
spec = do
  describe "Optimizer Property-Based Testing" $ do
    it "preserves program semantics after optimization" $
      property prop_optimizationPreservesSemantics
    
    it "produces correct results for expression optimization" $
      property prop_expressionOptimizationCorrect
    
    it "correctly optimizes constant expressions" $
      property prop_constExprOptimizationCorrect
    
    it "reduces or maintains program complexity after optimization" $
      property prop_optimizationReducesComplexity
    
    it "always terminates when optimizing expressions" $
      property prop_algebraicOptTerminates
      
    describe "Specific examples" $ do
      it "optimizes a Fibonacci program correctly" $ do
        -- Simple Fibonacci program
        let fibProg = Seq
              (Read "n")
              (Seq
                (Assign "a" (IntLit 0))
                (Seq
                  (Assign "b" (IntLit 1))
                  (Seq
                    (Assign "i" (IntLit 1))
                    (Seq
                      (While (Sub (VarLit "n") (VarLit "i"))
                        (Seq
                          (Assign "next" (Sum (VarLit "a") (VarLit "b")))
                          (Seq
                            (Assign "a" (VarLit "b"))
                            (Seq
                              (Assign "b" (VarLit "next"))
                              (Assign "i" (Sum (VarLit "i") (IntLit 1)))
                            )
                          )
                        )
                      )
                      (Write (VarLit "b"))
                    )
                  )
                )
              )
        
        let input = [6]  -- Calculate 6th Fibonacci number
        let originalOutput = runProgram fibProg input
        let optimizedProg = optimizeWithEmptySubst fibProg
        let optimizedOutput = runProgram optimizedProg input
        
        originalOutput `shouldBe` optimizedOutput
        originalOutput `shouldBe` [8]  -- F(6) = 8

      it "optimizes expressions with identities and constants" $ do
        -- Expression with multiple optimization opportunities
        let expr = Sum 
                    (Prod (IntLit 0) (VarLit "x")) 
                    (Sum 
                      (Prod (IntLit 1) (VarLit "y")) 
                      (Sub (VarLit "z") (IntLit 0)))
        
        let optimizedExpr = algebraicOpt expr
        let subst = M.fromList [("x", 10), ("y", 20), ("z", 30)]
        
        evalExpr expr subst `shouldBe` evalExpr optimizedExpr subst
        optimizedExpr `shouldBe` Sum (VarLit "y") (VarLit "z")
