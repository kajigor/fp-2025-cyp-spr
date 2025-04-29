{-# LANGUAGE OverloadedStrings #-}

module OptimizerPropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack)
import Control.Monad.State
import Data.List (nub)
import Debug.Trace
import Control.Exception (evaluate, try, SomeException, fromException, ArithException)
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import Data.Typeable (typeOf)

import Expression
import Statement
import Optimizer

-- Set this to True to enable more verbose logging
verboseLogging :: Bool
verboseLogging = False

-- Helper for conditional logging
logTrace :: String -> a -> a
logTrace msg = if verboseLogging then trace msg else id

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

-- | Generate list of inputs for program execution
genInputs :: Gen [Int]
genInputs = listOf $ choose (-100, 100)

-- | Generate non-empty list of inputs
genNonEmptyInputs :: Gen [Int]
genNonEmptyInputs = 
  sized $ \n -> do
    k <- choose (1, max 1 n)
    vectorOf k $ choose (-100, 100)

-- | Run a program with given inputs and return its output
runProgram :: Stmt -> [Int] -> [Int]
runProgram prog inputs = evalState (evalStmt prog) (M.empty, inputs)

-- Helper for consistent variable substitution generation
genSubst :: S.Set Text -> Gen (M.Map Text Int)
genSubst vars = do
  let varList = S.toList vars
  values <- vectorOf (length varList) (choose (-100, 100))
  return $ M.fromList (zip varList values)

-- Helper function to compare original and optimized expression evaluation results
compareExprResults :: Either SomeException Int -> Either SomeException Int -> String -> PropertyM IO Bool
compareExprResults originalResult optimizedResult context = case (originalResult, optimizedResult) of
  -- Both succeeded - values must match
  (Right val1, Right val2) -> 
    return (val1 == val2)
  
  -- Both failed - check exception types
  (Left e1, Left e2) -> 
    -- If original threw ArithException, optimized can do anything
    case fromException e1 :: Maybe ArithException of
      Just _ -> return True
      -- Otherwise, exception types should match
      Nothing -> return (show (typeOf e1) == show (typeOf e2))
  
  -- Original succeeded but optimized failed - this is an error
  (Right _, Left e2) -> do
    run (putStrLn $ "FAILED in " ++ context ++ ": Original succeeded but optimized failed with: " ++ show e2)
    return False
  
  -- Original failed with exception but optimized succeeded
  (Left e1, Right _) ->
    -- If original threw ArithException, optimization that avoids the exception is valid
    case fromException e1 :: Maybe ArithException of
      Just _ -> return True
      -- For other exception types, optimized shouldn't succeed
      Nothing -> do
        run (putStrLn $ "FAILED in " ++ context ++ ": Original failed with non-arithmetic exception but optimized succeeded")
        return False

-- | Property: Optimized expressions evaluate to the same value as the original
-- Also ensures division by zero exceptions are properly handled
prop_expressionOptimizationCorrect :: Expr -> Property
prop_expressionOptimizationCorrect expr =
  forAll (genSubst (getVarsInExpr expr)) $ \subst ->
    let vars = S.size (getVarsInExpr expr) in
    logTrace ("Testing expression optimization with " ++ show vars ++ " variables") $
      canEvalExpr expr subst ==>
        monadicIO $ do
          -- Try to evaluate the original expression
          res1 <- run $ try (evaluate (evalExpr expr subst)) :: PropertyM IO (Either SomeException Int)
          
          -- Try to evaluate the optimized expression
          let optimized = algebraicOpt expr
          res2 <- run $ try (evaluate (evalExpr optimized subst)) :: PropertyM IO (Either SomeException Int)
          
          -- Compare results
          result <- compareExprResults res1 res2 "algebraic optimization"
          assert result

-- | Property: Constant expression optimization is correct and preserves exceptions
prop_constExprOptimizationCorrect :: Expr -> Property
prop_constExprOptimizationCorrect expr =
  logTrace "Testing constant expression optimization" $
    monadicIO $ do
      -- Try to evaluate the original expression
      res1 <- run $ try (evaluate (evalExpr expr M.empty)) :: PropertyM IO (Either SomeException Int)
      
      -- Try to evaluate the optimized expression
      let optimized = optConstExpr expr
      res2 <- run $ try (evaluate (evalExpr optimized M.empty)) :: PropertyM IO (Either SomeException Int)
      
      -- Compare results
      result <- compareExprResults res1 res2 "constant expression optimization"
      assert result

-- | Property: Algebraic optimization terminates or throws ArithException divide by zero in case of illegal operation
prop_algebraicOptTerminates :: Expr -> Property
prop_algebraicOptTerminates expr =
  logTrace ("Testing termination on expression with size: " ++ show (sizeOfExpr expr)) $
    monadicIO $ do
      result <- run $ try (evaluate (algebraicOpt expr)) :: PropertyM IO (Either SomeException Expr)
      case result of
        Right _ -> assert True
        Left e ->
          case fromException e :: Maybe ArithException of
            Just _ -> assert True
            Nothing -> assert False
  where
    sizeOfExpr :: Expr -> Int
    sizeOfExpr (IntLit _) = 1
    sizeOfExpr (VarLit _) = 1
    sizeOfExpr (Sum a b) = 1 + sizeOfExpr a + sizeOfExpr b
    sizeOfExpr (Sub a b) = 1 + sizeOfExpr a + sizeOfExpr b
    sizeOfExpr (Prod a b) = 1 + sizeOfExpr a + sizeOfExpr b
    sizeOfExpr (Div a b) = 1 + sizeOfExpr a + sizeOfExpr b
    sizeOfExpr (Neg a) = 1 + sizeOfExpr a

-- | Calculate complexity of a program
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

-- Test utility that runs a program with fixed inputs and checks if optimization preserves semantics
testProgramWithFixedInputs :: String -> Stmt -> [Int] -> Spec
testProgramWithFixedInputs name prog inputs = 
  it ("preserves semantics of " ++ name ++ " program") $ do
    logTrace ("Testing " ++ name ++ " with inputs: " ++ show inputs) $ do
      let originalOutput = runProgram prog inputs
      let optimizedProg = optimizeWithEmptySubst prog
      let optimizedOutput = runProgram optimizedProg inputs
      
      -- Check semantics preservation
      originalOutput `shouldBe` optimizedOutput
      
      -- Check complexity reduction
      let origComplexity = complexity prog
      let optComplexity = complexity optimizedProg
      
      logTrace ("Complexity: " ++ show origComplexity ++ " -> " ++ 
               show optComplexity ++ " (reduction: " ++ 
               show (origComplexity - optComplexity) ++ ")") $ 
        optComplexity <= origComplexity `shouldBe` True

-- Predefined test programs with simpler while conditions

-- Fibonacci program: computes the nth Fibonacci number
fibonacciProgram :: Stmt
fibonacciProgram = Seq
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

-- Sum program: reads n and computes the sum of 1 to n
sumProgram :: Stmt
sumProgram = Seq
  (Read "n")
  (Seq
    (Assign "sum" (IntLit 0))
    (Seq
      (Assign "i" (IntLit 1))
      (Seq
        (While (Sub (VarLit "n") (VarLit "i"))
          (Seq
            (Assign "sum" (Sum (VarLit "sum") (VarLit "i")))
            (Assign "i" (Sum (VarLit "i") (IntLit 1)))
          )
        )
        (Write (VarLit "sum"))
      )
    )
  )

-- Factorial program: computes n!
factorialProgram :: Stmt
factorialProgram = Seq
  (Read "n")
  (Seq
    (Assign "fact" (IntLit 1))
    (Seq
      (Assign "i" (IntLit 1))
      (Seq
        (While (Sub (VarLit "n") (VarLit "i"))
          (Seq
            (Assign "i" (Sum (VarLit "i") (IntLit 1)))
            (Assign "fact" (Prod (VarLit "fact") (VarLit "i")))
          )
        )
        (Write (VarLit "fact"))
      )
    )
  )

-- Program with lots of algebraic optimization opportunities
algebraicOptimizationProgram :: Stmt
algebraicOptimizationProgram = Seq
  (Read "x")
  (Seq
    (Assign "y" (Sum (VarLit "x") (IntLit 0)))  -- y = x + 0 (simplifies to y = x)
    (Seq
      (Assign "z" (Prod (IntLit 1) (VarLit "y")))  -- z = 1 * y (simplifies to z = y)
      (Seq
        (Assign "a" (Prod (IntLit 0) (VarLit "z")))  -- a = 0 * z (simplifies to a = 0)
        (Seq
          (Assign "b" (Sum (VarLit "z") (VarLit "a")))  -- b = z + a (simplifies to b = z)
          (Write (VarLit "b"))
        )
      )
    )
  )

-- Program with constant folding opportunities
constantFoldingProgram :: Stmt
constantFoldingProgram = Seq
  (Read "x")
  (Seq
    (Assign "y" (Sum (IntLit 10) (IntLit 20)))  -- y = 10 + 20 (folds to y = 30)
    (Seq
      (Assign "z" (Prod (VarLit "y") (IntLit 2)))  -- z = y * 2 
      (Seq
        (Assign "a" (Sub (VarLit "z") (VarLit "x")))
        (Write (VarLit "a"))
      )
    )
  )

-- Program with a mixture of optimizations
mixedOptimizationsProgram :: Stmt
mixedOptimizationsProgram = Seq
  (Read "n")
  (Seq
    (Assign "a" (Sum (IntLit 5) (IntLit 3)))  -- a = 5 + 3 (folds to a = 8)
    (Seq
      (Assign "b" (Prod (VarLit "a") (IntLit 1)))  -- b = a * 1 (simplifies to b = a)
      (Seq
        (Assign "c" (Sum (VarLit "b") (IntLit 0)))  -- c = b + 0 (simplifies to c = b)
        (Seq
          (Assign "i" (IntLit 0))
          (Seq
            (While (Sub (VarLit "n") (VarLit "i"))
              (Seq
                (Assign "i" (Sum (VarLit "i") (IntLit 1)))
                (Assign "c" (Sum (VarLit "c") (IntLit 1)))
              )
            )
            (Write (VarLit "c"))
          )
        )
      )
    )
  )

-- Fixed test inputs for each program
fibonacciInputs :: [[Int]]
fibonacciInputs = [[6], [10], [1], [2], [100], [1000]]

sumInputs :: [[Int]]
sumInputs = [[5], [10], [1], [100], [1000]]

factorialInputs :: [[Int]]
factorialInputs = [[5], [1], [2], [3]]

algebraicInputs :: [[Int]]
algebraicInputs = [[42], [0], [10], [-5]]

constantFoldingInputs :: [[Int]]
constantFoldingInputs = [[5], [0], [-10], [20]]

mixedInputs :: [[Int]]
mixedInputs = [[5], [10], [1], [0]]

-- | Run tests with specific examples that demonstrate optimization benefits
spec :: Spec
spec = do
  describe "Expression Optimization Tests" $ do
    it "produces correct results for expression optimization" $
      withMaxSuccess 10000 $
      property prop_expressionOptimizationCorrect
    
    it "correctly optimizes constant expressions" $
      withMaxSuccess 10000 $
      property prop_constExprOptimizationCorrect
    
    it "always terminates when optimizing expressions" $
      withMaxSuccess 10000 $
      property prop_algebraicOptTerminates
      
  describe "Program Optimization Tests" $ do

    describe "Fibonacci tests" $ 
      mapM_ (testProgramWithFixedInputs "Fibonacci" fibonacciProgram) fibonacciInputs
    
    describe "Sum tests" $ 
      mapM_ (testProgramWithFixedInputs "Sum" sumProgram) sumInputs
    
    describe "Factorial tests" $ 
      mapM_ (testProgramWithFixedInputs "Factorial" factorialProgram) factorialInputs
    
    describe "Algebraic Optimization tests" $ 
      mapM_ (testProgramWithFixedInputs "Algebraic Optimization" algebraicOptimizationProgram) algebraicInputs
    
    describe "Constant Folding tests" $ 
      mapM_ (testProgramWithFixedInputs "Constant Folding" constantFoldingProgram) constantFoldingInputs
    
    describe "Mixed Optimizations tests" $ 
      mapM_ (testProgramWithFixedInputs "Mixed Optimizations" mixedOptimizationsProgram) mixedInputs
