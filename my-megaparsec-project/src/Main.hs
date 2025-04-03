{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (void)
import Data.Char (isDigit)

type Parser = Parsec Void Text

data Expr
  = IntLit Int
  | Sum Expr Expr
  | Prod Expr Expr
  deriving (Eq, Show)

parseInt :: Parser Expr
parseInt = do
  digits <- some (satisfy isDigit)
  return (IntLit (read digits))

parseSequence :: Char -> Parser Expr -> Parser [Expr]
parseSequence sep p = do
  first <- p
  rest <- many (do
    void (char sep)
    p)
  return (first : rest)

parseProdExpr :: Parser Expr
parseProdExpr = do
  args <- parseSequence '*' parseInt
  return $ foldl1 Prod args

parseSumExpr :: Parser Expr
parseSumExpr = do
  args <- parseSequence '+' parseProdExpr
  return $ foldl1 Sum args

parseExpr :: Parser Expr
parseExpr = parseSumExpr

main :: IO ()
main = do
  let input = "1+2*3*4+2"
  let result = runParser parseExpr "" (T.pack input)
  print result

