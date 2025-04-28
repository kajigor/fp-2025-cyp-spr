{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( Parser
  , spaceConsumer
  , symbol
  , lexeme
  , integer
  , parens
  , identifier
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Char (isAlphaNum, isAlpha)

-- | Type synonym for the parser
type Parser = Parsec Void Text

-- | Space consumer: skips whitespace
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- | Parse a specific character and consume trailing whitespace
symbol :: Char -> Parser Char
symbol c = char c <* spaceConsumer

-- | Apply a parser and consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse an integer and consume trailing whitespace
integer :: Parser Int
integer = lexeme L.decimal

-- | Parse something between parentheses
parens :: Parser a -> Parser a
parens p = between (symbol '(') (symbol ')') p

-- | Parse an identifier (letter followed by alphanumeric characters)
identifier :: Parser Text
identifier = lexeme $ do
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum)
  return $ T.pack (first : rest)