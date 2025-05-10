module Md2HtmlParser.Parser.Utils
  ( Parser,
    spaceConsumer,
    lexeme,
    integer,
    parens,
    curly,
    identifier,
    keyword,
    -- Markdown-specific utilities
    endOfLine,
    endOfInput,
    blankLine,
    blankLines,
    nonIndentedLine,
    indentedLine,
    takeRestOfLine,
    manyTill1,
    takeWhileP1,
    brackets,
    square,
    optionalBlankLines,
    withManyTill,
    isSpecialChar,
    notSpecialChar,
    lineWithPrefix,
    takeUntilSpecialOrNewline,
    takeUntilSpecialOrNewlineOrEmpty,
    betweenChars,
    textString,
    indented,
  )
where

import Control.Monad (guard, void)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import GHC.TypeLits ()

-- | Type synonym for the parser
type Parser = Parsec String Text

-- | Space consumer: skips whitespace
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- | Apply a parser and consume trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- | Parse an integer and consume trailing whitespace
integer :: Parser Int
integer = lexeme L.decimal <?> "integer"

-- | Parse something between parentheses
parens :: Parser a -> Parser a
parens parser = do
  _ <- char '(' <?> "opening parenthesis"
  content <- parser
  _ <- char ')' <?> "closing parenthesis"
  return content

-- | Parse something between curly braces
curly :: Parser a -> Parser a
curly parser = do
  _ <- char '{' <?> "opening curly brace"
  content <- parser
  _ <- char '}' <?> "closing curly brace"
  return content

-- | Parse something between square brackets
square :: Parser a -> Parser a
square parser = do
  _ <- char '[' <?> "opening square bracket"
  content <- parser
  _ <- char ']' <?> "closing square bracket"
  return content

-- | General brackets parser
brackets :: Char -> Char -> Parser a -> Parser a
brackets open close parser = do
  _ <- char open <?> ("opening " ++ [open])
  content <- parser
  _ <- char close <?> ("closing " ++ [close])
  return content

betweenChars :: Char -> Parser Text -> Parser Text
betweenChars c parser = do
  _ <- char c
  content <- parser
  _ <- char c
  return content

-- | Parse an identifier (letter followed by alphanumeric characters)
identifier :: Parser Text
identifier = lexeme $ do
  first <- satisfy isAlpha <?> "letter"
  rest <- many (satisfy isAlphaNum <?> "alphanumeric character")
  return (T.pack (first : rest)) <?> "identifier"

-- | Parse a keyword (expected to be equal given one)
keyword :: Text -> Parser Text
keyword kw = lexeme $ try $ do
  string kw <?> ("keyword \"" ++ T.unpack kw ++ "\"")
  notFollowedBy alphaNumChar

  return kw

-- | Parse a newline character
endOfLine :: Parser Char
endOfLine = char '\n' <?> "end of line"

-- | Parse end of input
endOfInput :: Parser ()
endOfInput = eof <?> "end of input"

-- | Parse a blank line (only whitespace followed by newline)
blankLine :: Parser ()
blankLine = try $ do
  skipMany (satisfy isSpace)
  endOfLine
  return () <?> "blank line"

-- | Parse one or more blank lines
blankLines :: Parser ()
blankLines = do
  some blankLine
  return ()
  <?> "blank lines"

-- | Parse zero or more blank lines
optionalBlankLines :: Parser ()
optionalBlankLines = do
  many blankLine
  return ()
  <?> "optional blank lines"

-- | Parse a line that is not indented
nonIndentedLine :: Parser Text
nonIndentedLine = takeWhileP1 (/= '\n') <* endOfLine <?> "non-indented line"

-- | Parse a line with a specific indentation level
indentedLine :: Int -> Parser Text
indentedLine n = try $ do
  _ <- count n (char ' ' <?> "space")
  takeWhileP1 (/= '\n') <* endOfLine <?> ("line with " ++ show n ++ " spaces indentation")

-- | Parse a line with a specific indentation level by the provided parser
indented :: Int -> Parser a -> Parser a
indented n parser = try $ do
  _ <- count n (char ' ' <?> "space") <|> count n (char '\t' <?> "tab")
  parser

-- | Take the rest of the line until newline or end of input
takeRestOfLine :: Parser Text
takeRestOfLine = takeWhileP (Just "rest of line") (/= '\n') <* endOfLine <?> "rest of line"

-- | Like manyTill but requires at least one occurrence
manyTill1 :: Parser a -> Parser end -> Parser [a]
manyTill1 p end = do
  x <- p <?> "first element"
  xs <- manyTill (p <?> "subsequent element") end
  return (x : xs)
  <?> "at least one element"

-- | Like takeWhileP but requires at least one occurrence
takeWhileP1 :: (Char -> Bool) -> Parser Text
takeWhileP1 predicate = do
  c <- satisfy predicate <?> "matching character"
  rest <- takeWhileP (Just "subsequent characters") predicate
  return $ T.cons c rest
  <?> "at least one character"

-- | Utility for parser that collects results
withManyTill :: Parser a -> Parser end -> ([a] -> b) -> Parser b
withManyTill p end f = f <$> manyTill p end

-- | Check if a character is a special Markdown character
isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` ("*_`[]()!#+|-" :: String)

-- | Check if a character is not a special Markdown character
notSpecialChar :: Char -> Bool
notSpecialChar = not . isSpecialChar

-- | Parse a line with a specific prefix
lineWithPrefix :: Text -> Parser Text
lineWithPrefix prefix = try $ do
  string prefix <?> ("line prefix \"" ++ T.unpack prefix ++ "\"")
  takeRestOfLine <?> ("line with prefix \"" ++ T.unpack prefix ++ "\"")

-- | Take text until first special character or newline
takeUntilSpecialOrNewline :: Parser Text
takeUntilSpecialOrNewline = takeWhileP1 (\c -> not (isSpecialChar c || c == '\n'))
                          <?> "text without special characters or newlines"

-- | Take text until first special character or newline
takeUntilSpecialOrNewlineOrEmpty :: Parser Text
takeUntilSpecialOrNewlineOrEmpty = takeWhileP (Just "subsequent characters") (\c -> not (isSpecialChar c || c == '\n'))
                          <?> "text without special characters or newlines"

-- | Parse a string and return it as Text
textString :: String -> Parser Text
textString s = string (T.pack s) <?> ("string \"" ++ s ++ "\"")
