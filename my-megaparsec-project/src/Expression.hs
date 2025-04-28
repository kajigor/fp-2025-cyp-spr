{-# LANGUAGE OverloadedStrings #-}

module Expression
  ( Expr(..)
  , evalExpr
  , prettyPrintExpr
  , prettyPrintParen
  , parseExpr
  , parseTerm
  , parseFactor
  , parseParens
  , parseNegative
  , parseInt
  , symbol
  , spaceConsumer
  , Parser
  , Subst
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M

type Parser = Parsec Void Text
type Subst = M.Map Text Int

data Expr
  = IntLit Int
  | VarLit Text
  | Sum Expr Expr
  | Sub Expr Expr
  | Prod Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Eq, Show)

evalExpr :: Expr -> Subst -> Int
evalExpr (IntLit n) _ = n
evalExpr (VarLit var) state = state M.! var
evalExpr (Sum a b) state = evalExpr a state + evalExpr b state
evalExpr (Sub a b) state = evalExpr a state - evalExpr b state
evalExpr (Prod a b) state = evalExpr a state * evalExpr b state
evalExpr (Div a b) state = evalExpr a state `div` evalExpr b state
evalExpr (Neg a) state = -(evalExpr a) state

prettyPrintExpr :: Expr -> String
prettyPrintExpr (IntLit n) = show n
prettyPrintExpr (VarLit var) = show var
prettyPrintExpr (Sum a b)  = prettyPrintExpr a ++ " + " ++ prettyPrintExpr b
prettyPrintExpr (Sub a b)  = prettyPrintExpr a ++ " - " ++ prettyPrintParen b
prettyPrintExpr (Prod a b) = prettyPrintParen a ++ " * " ++ prettyPrintParen b
prettyPrintExpr (Div a b)  = prettyPrintParen a ++ " / " ++ prettyPrintParen b
prettyPrintExpr (Neg a) = "-" ++ prettyPrintExpr a

prettyPrintParen :: Expr -> String
prettyPrintParen e = case e of
  IntLit _ -> prettyPrintExpr e
  _        -> "(" ++ prettyPrintExpr e ++ ")"

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

symbol :: Char -> Parser Char
symbol c = char c <* spaceConsumer

parseInt :: Parser Expr
parseInt = do
  n <- L.decimal <* spaceConsumer
  return (IntLit n)

parseNegative :: Parser Expr
parseNegative = do
  _ <- symbol '-'
  Neg <$> (parseInt <|> parseExpr)

parseParens :: Parser Expr
parseParens = do
  _ <- symbol '('
  expr <- parseExpr
  _ <- symbol ')'
  return expr

parseFactor :: Parser Expr
parseFactor = parseInt <|> parseNegative <|> parseParens

-- parseTerm = chain of factors, split by * or /
parseTerm :: Parser Expr
parseTerm = do
  first <- parseFactor
  rest <- many $ do
    op <- (symbol '*' >> return Prod) <|> (symbol '/' >> return Div)
    expr <- parseFactor
    return (op, expr)
  return $ foldl (\acc (op, x) -> op acc x) first rest

-- parseExpr = chain of Terms, split by + or -
parseExpr :: Parser Expr
parseExpr = do
  first <- parseTerm
  rest <- many $ do
    op <- (symbol '+' >> return Sum) <|> (symbol '-' >> return Sub)
    expr <- parseTerm
    return (op, expr)
  return $ foldl (\acc (op, x) -> op acc x) first rest
