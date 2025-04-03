{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
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

evalExpr :: Expr -> Int
evalExpr (IntLit n) = n
evalExpr (Sum a b) = evalExpr a + evalExpr b
evalExpr (Sub a b) = evalExpr a - evalExpr b
evalExpr (Prod a b) = evalExpr a * evalExpr b
evalExpr (Div a b) = evalExpr a `div` evalExpr b

prettyPrintExpr :: Expr -> String
prettyPrintExpr (IntLit n) = show n
prettyPrintExpr (Sum a b)  = prettyPrintExpr a ++ " + " ++ prettyPrintExpr b
prettyPrintExpr (Sub a b)  = prettyPrintExpr a ++ " - " ++ prettyPrintParen b
prettyPrintExpr (Prod a b) = prettyPrintParen a ++ " * " ++ prettyPrintParen b
prettyPrintExpr (Div a b)  = prettyPrintParen a ++ " / " ++ prettyPrintParen b

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
  IntLit value <- parseInt
  return (IntLit (-value))

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

runTest :: String -> Int -> IO ()
runTest input expected =
  case runParser parseExpr "" (T.pack input) of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right expr -> do
      print expr
      let computed = evalExpr expr
      let passed = computed == expected
      putStrLn $
        prettyPrintExpr expr ++ " == " ++ show computed ++ " <- " ++ show passed ++ "\n"

main :: IO ()
main = do
  runTest "1 + 2 * 3" 7
  runTest "-1 + 2 * 3" 5
  runTest "-(1 + 2) * 3" (-9)
  runTest "10 / 2 + 1" 6
  runTest "2 * (3 + 4)" 14
  runTest "1 + 2 + 3 + 4" 10
  runTest "18 / 3 / 3" 2
  runTest "-3 * -2" 6
  runTest "-3 * (2 + 1)" (-9)
  runTest "-1 + 2 *3 * 4 +-2" 21
  runTest "((-1) + (2 * 3) * 4 + (-2))" 21
  runTest "((-1) + (2 * 3) * 4 + (-2))" 21
  runTest "(-1 + 2) * 3 * (4 + (2))" 18