{-# LANGUAGE OverloadedStrings #-}

module ExpressionSpec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Data.Text as T (pack)
import Text.Megaparsec
import qualified Data.Map as M

import Expression

-- Helper function to test expression evaluation
testExpr :: String -> Int -> Expectation
testExpr input expected = do
  case runParser parseExpr "" (T.pack input) of
    Left err -> expectationFailure $ "Parse error: " ++ errorBundlePretty err
    Right expr -> do
      let computed = evalExpr expr M.empty
      computed `shouldBe` expected

-- Helper function to test parsing success
shouldParseTo :: String -> Expr -> Expectation
shouldParseTo input expected = 
  parse parseExpr "" (T.pack input) `shouldParse` expected

spec :: Spec
spec = do
  describe "Expression Parser" $ do
    context "basic arithmetic" $ do
      it "parses and evaluates simple addition and multiplication" $ do
        testExpr "1 + 2 * 3" 7
        testExpr "10 / 2 + 1" 6
        testExpr "1 + 2 + 3 + 4" 10
        
      it "handles negative numbers correctly" $ do
        testExpr "-1 + 2 * 3" 5
        testExpr "-3 * -2" 6
        testExpr "-3 * (2 + 1)" (-9)
        
      it "follows order of operations with parentheses" $ do
        testExpr "-(1 + 2) * 3" (-9)
        testExpr "2 * (3 + 4)" 14
        testExpr "((-1) + (2 * 3) * 4 + (-2))" 21
        
      it "handles complex expressions with multiple operations" $ do
        testExpr "18 / 3 / 3" 2
        testExpr "-1 + 2 *3 * 4 +-2" 21
        testExpr "(-1 + 2) * 3 * (4 + (2))" 18
        testExpr "(-(1 + 2) * 3) * (4 + (-2))" (-18)
      
    context "parser structure" $ do
      it "correctly builds expression AST" $ do
        "1 + 2" `shouldParseTo` Sum (IntLit 1) (IntLit 2)
        "1 * 2" `shouldParseTo` Prod (IntLit 1) (IntLit 2)
        "-1" `shouldParseTo` Neg (IntLit 1)
        "1 + 2 * 3" `shouldParseTo` Sum (IntLit 1) (Prod (IntLit 2) (IntLit 3))
