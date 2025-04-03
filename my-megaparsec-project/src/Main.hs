{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (void)
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Expr
  = IntLit Int
  | Sum Expr Expr
  | Sub Expr Expr
  | Prod Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

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
  Sub (IntLit 0) <$> parseInt

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

-- parseExpr = cahin of Terms, split + by -
parseExpr :: Parser Expr
parseExpr = do
  first <- parseTerm
  rest <- many $ do
    op <- (symbol '+' >> return Sum) <|> (symbol '-' >> return Sub)
    expr <- parseTerm
    return (op, expr)
  return $ foldl (\acc (op, x) -> op acc x) first rest

main :: IO ()
main = do
  print $ runParser parseExpr "" (T.pack "-1 + 2 *3 * 4 +-2")
  print $ runParser parseExpr "" (T.pack "((-1) + (2 * 3) * 4 + (-2))")
  print $ runParser parseExpr "" (T.pack "(-1 + 2) * 3 * (4 + (2))")
