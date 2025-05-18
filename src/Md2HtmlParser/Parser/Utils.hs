module Md2HtmlParser.Parser.Utils
  ( Parser,
    countedSpaceConsumer,
    parens,
    curly,
    -- Markdown-specific utilities
    endOfLine,
    takeWhileP1,
    square,
    isSpecialChar,
    takeUntilSpecialOrNewline,
    takeUntilAllowedInLink,
    takeUntilSpecialOrNewlineOrEmpty,
    textString,
    indented,
    char,
    takeWhileP,
    manyTill,
    endOfFile,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec (ParsecT, empty, (<?>), some, try, count, (<|>))
import Control.Monad.State.Strict
import Md2HtmlParser.Metrics (Metrics, Parser)

-- Import for our instrumented 'char' function that uses the counter
import Md2HtmlParser.Metrics (charWithCount, incByN)

-- | Custom char function that wraps charWithCount for metrics.
-- This is instrumented and will increment Metrics.charCallCounter.
char :: Char -> Parser Char
char = charWithCount

countedSpaceChar :: Parser Char
countedSpaceChar = char ' ' <?> "space"

countedSpace1 :: Parser ()
countedSpace1 = void (some countedSpaceChar) <?> "spaces"

-- Uses 'empty' for line and block comments as Markdown doesn't have them.
countedSpaceConsumer :: Parser ()
countedSpaceConsumer = L.space countedSpace1 empty empty <?> "whitespace (counted attempt)"

-- | Helper for countedStringParser
countedStringParser :: Text -> Parser Text
countedStringParser s = M.try (T.pack <$> mapM char (T.unpack s)) <?> ("counted string: " ++ T.unpack s)

textString :: String -> Parser Text
textString s = countedStringParser (T.pack s) <?> ("string \"" ++ s ++ "\"")

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

-- | Parse a newline character
endOfLine :: Parser Char
endOfLine = char '\n' <?> "end of line"

endOfFile :: Parser ()
endOfFile = M.eof >>= incByN 1 <?> "end of file"

indented :: Int -> Parser a -> Parser a
indented n parser = try $ do
  _ <- count n (char ' ' <?> "space") <|> count n (char '\t' <?> "tab")
  parser

-- | Consumes and returns a block of text where each character satisfies the predicate.
takeWhileP :: (Char -> Bool) -> Parser Text
takeWhileP predicate =
  T.pack <$> M.many (M.try $ M.satisfy predicate >>= incByN 1) <?> "countedTakeWhileP"

-- | Like takeWhileP but requires at least one occurrence
takeWhileP1 :: (Char -> Bool) -> Parser Text
takeWhileP1 predicate = do
  c <- M.satisfy predicate >>= incByN 1 <?> "matching character"
  rest <- takeWhileP predicate
  return $ T.cons c rest

manyTill :: Parser a
                -> Parser end
                -> Parser [a]
manyTill p = M.manyTill (countedP p)
  where
    countedP :: Parser a -> Parser a
    countedP innerP = do
      startOffset <- M.getOffset
      res <- innerP
      endOffset <- M.getOffset
      incByN (endOffset - startOffset) res


-- | Check if a character is a special Markdown character
isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` ("*_`[]!#+|" :: String)

-- | Take text until first special character or newline
takeUntilSpecialOrNewline :: Parser Text
takeUntilSpecialOrNewline = takeWhileP1 (\c -> not (isSpecialChar c || c == '\n'))
                          <?> "text without special characters or newlines"

-- | Take symbols allowed inside links
takeUntilAllowedInLink :: Parser Text
takeUntilAllowedInLink = takeWhileP1 (\c -> not (c `elem` ("`[](){}<>\"'|^ " :: String) || c == '\n'))
                          <?> "text without special characters or newlines inside link"

-- | Take text until first special character or newline
takeUntilSpecialOrNewlineOrEmpty :: Parser Text
takeUntilSpecialOrNewlineOrEmpty = takeWhileP (\c -> not (isSpecialChar c || c == '\n'))
                          <?> "text without special characters or newlines"