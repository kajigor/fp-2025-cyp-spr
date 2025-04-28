{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Data.Char
import Control.Applicative

type Error = String

newtype Parser a = Parser { runParser :: String -> Either Error (String, a) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \inp ->
    case p inp of
      Left err -> Left err
      Right (inp', x) -> Right (inp', f x)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \inp -> Right (inp, x)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser f <*> Parser p = Parser $ \inp ->
    case f inp of
      Right (inp', f' :: a -> b) ->
        case p inp' of
          Right (inp'', x :: a) -> Right (inp'', f' x)
          Left err -> Left err
      Left err -> Left err

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \inp ->
    case p inp of
      Left err -> Left err
      Right (inp', x) -> runParser (f x) inp'

-- runParser (satisfy isDigit) "123" -> Right ("23", '1')
-- runParser (satisfy isDigit) "x123" -> Left "..."
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \inp ->
  case inp of
    ( h : t ) | p h -> Right (t, h)
    _ -> Left $ "satisfy failed on " ++ inp

-- Ident = Alpha AlphaNum*
parseIdent :: Parser String
parseIdent = do
    h <- satisfy isAlpha
    t <- go
    return (h:t)
  where
    go = (do
        x <- satisfy isAlphaNum
        y <- go
        return (x:y))
      <|>
        return []

instance Alternative Parser where
  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p <|> Parser q = Parser $ \inp ->
    case p inp of
      Left err -> q inp
      Right (inp', x) -> Right (inp', x)

  empty :: Parser a
  empty = Parser $ \inp -> Left "empty"
  -- empty <|> x = x
  -- x <|> empty x

isSpaceOrComma :: Char -> Bool
isSpaceOrComma c = isSpace c || c == ','

parseSpaces :: Parser ()
parseSpaces = do
  _ <- many (satisfy isSpace)
  return ()

parseSeps :: Parser ()
parseSeps = do
  _ <- many (satisfy isSpaceOrComma)
  return ()

parseInt :: Parser Int
parseInt = do
  digits <- many (satisfy isDigit)
  if null digits
     then empty
     else return (read digits)

parseElem :: Parser ()
parseElem = do
  elem <- (parseInt >> pure ()) <|> parseTuple
  return elem

--parseElems :: Parser ()
--parseElems = do
--  elems <- many parseElem
--  return ()

parseElems :: Parser ()
parseElems = do
  _ <- parseElem
  _ <- many (parseSeps >> parseElem)
  return ()

parseTuple :: Parser ()
parseTuple = do
  _ <- satisfy (== '(')
  _ <- parseElems <|> pure ()
  _ <- satisfy (== ')')
  return ()

data Expr
  = Int Int
  | Sum Expr Expr
  | Prod Expr Expr
   deriving (Eq, Show)

parseSequence :: Char -> Parser a -> Parser [a]
parseSequence sep p = do
  first <- p
  rest <- many (do
    _ <- satisfy (== sep)
    p)
  return (first : rest)

parseSumExpr = do
  args <- parseSequence '+' parseInt
  case map Int args of
    [x] -> return x
    xs -> return $ foldl1 Sum xs

parseMutExpt = do
  args <- parseSequence '*' parseInt
  case map Int args of
    [x] -> return x
    xs -> return $ foldl1 Prod xs

parseExpr = do
  args <- parseSequence '+' parseMutExpt
  case args of
    [x] -> return x
    xs -> return $ foldl1 Sum xs

main :: IO ()
main = do
--  print $ runParser parseTuple "((), 1, 2, 3, (1))"
  print$ runParser parseExpr "1+2*3*4+2"
