{-# LANGUAGE OverloadedStrings #-}

module Statement
  ( Stmt(..)
  , evalStmt
  , prettyPrintStmt
  , parseStmt
  ) where

import Data.Text (Text)
import Control.Monad.State
import qualified Data.Map as M
import Control.Applicative ((<|>))
import Text.Megaparsec (many, try)

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
prettyPrintStmt (Read var) = show var ++ " = readInt();"

{-
My aim is to start parsing simple stgatement C like language:
Simple Fibonacci Program:

n = readInt();
a = 0;
b = 1;
i = 1;
while ((n - i)) {
  next = (a + b);
  a = b;
  b = next;
  i = (i + 1);
};
print(b);

actually readInt and print not "reads and writes Ints",
 but takes and put thems into In / Out list"
-}

parseBody :: Parser Stmt
parseBody = curly parseStmt

readInt :: Parser Text
readInt = lexeme $ do
  _ <- keyword "readInt"
  _ <- symbol '('
  _ <- symbol ')'
  return "readInt()"


parseAssign :: Parser Stmt
parseAssign = do
  var <- identifier
  _ <- symbol '='
  Assign var <$> parseExpr

parseWhile :: Parser Stmt
parseWhile = do
  _ <- keyword "while"
  cond <- parens parseExpr
  While cond <$> parseBody

parseSeq :: Parser Stmt
parseSeq = do
  first <- parseStmt
  Seq first <$> parseStmt

parseWrite :: Parser Stmt
parseWrite = do
  _ <- keyword "print"
  expr <- parens parseExpr
  return $ Write expr

parseRead :: Parser Stmt
parseRead = do
  var <- identifier
  _ <- symbol '='
  _ <- readInt
  return $ Read var

-- | Parse a semicolon and optionally consume newlines after it
semicolon :: Parser ()
semicolon = do
  _ <- symbol ';'
  _ <- many (symbol '\n')  -- Consume any number of newlines after semicolon
  return ()


parseStmt :: Parser Stmt
parseStmt = do
  stmt <- try parseRead <|> parseWrite <|> parseWhile <|> try parseAssign <|> parseSeq
  _ <- semicolon
  return stmt
