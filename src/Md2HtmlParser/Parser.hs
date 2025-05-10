module Md2HtmlParser.Parser
  ( parseMarkdownElement,
    parseMarkdownDoc,
    MarkdownDoc (..),
    MarkdownElement (..),
    InlineElement (..),
    parseInlineElement,
    parseImageText,
    parseCodeText,
    parseLinkText,
    parsePlainText,
    parseBold,
    parseItalic,
    parseNumberedList,
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Md2HtmlParser.Logger (logParserCall, logParserResult)
import Md2HtmlParser.Parser.Utils
  ( Parser,
    endOfLine,
    indented,
    parens,
    square,
    takeUntilSpecialOrNewline,
    takeUntilSpecialOrNewlineOrEmpty,
    textString,
  )
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
  ( anySingle,
    choice,
    eof,
    getInput,
    many,
    optional,
    satisfy,
    some,
    takeWhileP,
    try,
    (<?>),
    (<|>),
    manyTill,
  )
import Text.Megaparsec.Char (char, string)

-- | Logs parser execution and returns the result
withLogs :: Show a => String -> Parser a -> Parser a
withLogs name parser = do
  -- Get a preview of the current input before parsing
  inputPreview <- showCurrentInput
  -- Log the parser call
  seq (unsafePerformIO $ logParserCall name inputPreview) $ do
    -- Run the actual parser
    result <- parser
    -- Get the remaining input after parsing
    remaining <- showCurrentInput
    -- Log the result and remaining input
    seq (unsafePerformIO $ logParserResult name result remaining) $ return result

-- | Helper function to safely get a string representation of the current input
showCurrentInput :: Parser String
showCurrentInput = do
  s <- getInput
  -- Take at most 60 characters to avoid overwhelming logs
  return $ take 60 (T.unpack s) ++ (if T.length s > 60 then "..." else "")

-- | Represents a markdown document
newtype MarkdownDoc = MarkdownDoc [MarkdownElement]
  deriving (Show, Eq)

-- | Inline markdown content that can be nested
data InlineElement
  = PlainText Text
  | ItalicText [InlineElement]
  | BoldText [InlineElement]
  | CodeText Text
  | LinkText [InlineElement] Text
  | -- | Image with alt text and URL
    ImageText Text Text
  deriving (Show, Eq)

-- | Different elements that can appear in a markdown document
data MarkdownElement
  = -- | Header with level and text
    Header Int [InlineElement]
  | -- | Paragraph of text
    Paragraph [InlineElement]
  | CodeBlock (Maybe Text) Text
  | -- | Bulleted list of items with potential formatting
    BulletList [[InlineElement]]
  | -- | Numbered list of items with potential formatting
    NumberedList [[InlineElement]]
  | HorizontalRule
  | EmptyLine
  | -- | Block quote that can contain other markdown elements
    BlockQuote [MarkdownElement]
  deriving (Show, Eq)

parsePlainText :: Parser InlineElement
parsePlainText = withLogs "parsePlainText" $
  PlainText <$> takeUntilSpecialOrNewline

parseItalic :: Parser InlineElement
parseItalic = withLogs "parseItalic" $ do
  start <- try (textString "_") <|> try (textString "*")
  content <- try (some parseInlineElement)
  _ <- textString (Text.unpack start)
  return $ ItalicText content

parseBold :: Parser InlineElement
parseBold = withLogs "parseBold" $ do
  start <- try (textString "__") <|> try (textString "**")
  content <- try (some parseInlineElement)
  _ <- textString (Text.unpack start)
  return $ BoldText content

parseCodeText :: Parser InlineElement
parseCodeText = withLogs "parseCodeText" $ do
  _ <- char '`'
  content <- takeWhileP (Just "code text") (\c -> not (c == '`' || c == '\n'))
  _ <- char '`'
  return $ CodeText content

parseLinkText :: Parser InlineElement
parseLinkText = withLogs "parseLinkText" $ do
  text <- square (many parseInlineElement)
  url <- parens takeUntilSpecialOrNewline
  return $ LinkText text url

parseImageText :: Parser InlineElement
parseImageText = withLogs "parseImageText" $ do
  _ <- char '!'
  text <- square takeUntilSpecialOrNewlineOrEmpty
  url <- parens takeUntilSpecialOrNewline
  return $ ImageText text url

parseInlineElement :: Parser InlineElement
parseInlineElement = withLogs "parseInlineElement" $
  choice
    [ parseImageText,
      parseLinkText,
      try parseCodeText,
      try parseBold,
      try parseItalic,
      parsePlainText
    ]

parseMdHeader :: Parser MarkdownElement
parseMdHeader = withLogs "parseMdHeader" $ do
  level <- length <$> some (char '#')
  _ <- some (char ' ')
  content <- many parseInlineElement
  _ <- endOfLine *> pure () <|> eof
  return $ Header level content

parseParagraph :: Parser MarkdownElement
parseParagraph = withLogs "parseParagraph" $ do
  content <- some parseInlineElement
  _ <- endOfLine *> pure () <|> eof
  return $ Paragraph content

parseCodeBlock :: Parser MarkdownElement
parseCodeBlock = withLogs "parseCodeBlock" $ do
  _ <- string (Text.pack "```")
  lang <- optional $ takeWhileP (Just "language") (\c -> not (c == '\n' || c == ' '))
  _ <- endOfLine
  content <-
    manyTill
      anySingle
      ( try $ do
          _ <- endOfLine
          _ <- string (Text.pack "```")
          return ()
      )
  _ <- endOfLine *> pure () <|> eof
  let language = case lang of
        Just l | not (T.null l) -> Just (T.stripEnd l)
        _ -> Nothing
  return $ CodeBlock language (T.pack content)

parseBulletListInlineElem :: Parser [InlineElement]
parseBulletListInlineElem = withLogs "parseBulletListInlineElem" $ do
  _ <- (char '*' <?> "* point") <|> (char '-' <?> "- point")
  _ <- char ' ' <?> "space"

  many parseInlineElement

parseBulletList :: Parser MarkdownElement
parseBulletList = withLogs "parseBulletList" $ do
  level <- length <$> many (char ' ')
  firstItem <- parseBulletListInlineElem
  restItems <-  many $ try $ do
    _ <- endOfLine
    indented level parseBulletListInlineElem
  _ <- endOfLine *> pure () <|> eof
  return $ BulletList (firstItem : restItems)

parseNumberedListInlineElem :: Parser [InlineElement]
parseNumberedListInlineElem = withLogs "parseNumberedListInlineElem" $ do
  _ <- some (satisfy isDigit) <?> "digit"
  _ <- char '.' <?> "period"
  _ <- char ' ' <?> "space"

  many parseInlineElement

parseNumberedList :: Parser MarkdownElement
parseNumberedList = withLogs "parseNumberedList" $ do
  level <- length <$> many (char ' ')
  firstItem <- parseNumberedListInlineElem
  restItems <- many $ try $ do
    _ <- endOfLine
    indented level parseNumberedListInlineElem
  _ <- endOfLine *> pure () <|> eof
  return $ NumberedList (firstItem : restItems)

parseHorizontalRule :: Parser MarkdownElement
parseHorizontalRule = withLogs "parseHorizontalRule" $ do
  _ <- string (Text.pack "---")
  _ <- endOfLine *> pure () <|> eof
  return HorizontalRule

parseEmptyLine :: Parser MarkdownElement
parseEmptyLine = withLogs "parseEmptyLine" $ EmptyLine <$ endOfLine

-- | Parse markdown text into a MarkdownDoc structure
parseMarkdownElement :: Parser MarkdownElement
parseMarkdownElement =
  withLogs "parseMarkdownElement" $
    choice
      [ parseMdHeader,
        parseCodeBlock,
        try parseBulletList,
        try parseNumberedList,
        parseHorizontalRule,
        parseParagraph,
        parseEmptyLine
      ]

parseMarkdownDoc :: Parser MarkdownDoc
parseMarkdownDoc = withLogs "parseMarkdownDoc" $ do
  content <- many parseMarkdownElement
  return $ MarkdownDoc content
