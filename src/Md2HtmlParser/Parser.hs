module Md2HtmlParser.Parser
  ( parseMarkdownElement,
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
  )
where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text as Text
import Md2HtmlParser.Parser.Utils
  ( Parser,
    endOfLine,
    indented,
    manyTill1,
    parens,
    square,
    symbol,
    takeUntilSpecialOrNewline,
    textString,
  )
import Text.Megaparsec
  ( anySingle,
    choice,
    eof,
    many,
    optional,
    some,
    takeWhileP,
    try,
    (<|>),
    (<?>),
    satisfy,
  )
import Text.Megaparsec.Char (char, string)

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
parsePlainText = PlainText <$> takeUntilSpecialOrNewline

parseItalic :: Parser InlineElement
parseItalic = do
  start <- try (textString "_") <|> try (textString "*")
  content <- try (some parseInlineElement)
  _ <- textString (Text.unpack start)
  return $ ItalicText content

parseBold :: Parser InlineElement
parseBold = do
  start <- try (textString "__") <|> try (textString "**")
  content <- try (some parseInlineElement)
  _ <- textString (Text.unpack start)
  return $ BoldText content

parseCodeText :: Parser InlineElement
parseCodeText = do
  _ <- symbol '`'
  content <- takeWhileP (Just "code text") (\c -> c /= '`' && c /= '\n')
  _ <- symbol '`'
  return $ CodeText content

parseLinkText :: Parser InlineElement
parseLinkText = do
  text <- square (many parseInlineElement)
  url <- parens takeUntilSpecialOrNewline
  return $ LinkText text url

parseImageText :: Parser InlineElement
parseImageText = do
  _ <- symbol '!'
  text <- square takeUntilSpecialOrNewline
  url <- parens takeUntilSpecialOrNewline
  return $ ImageText text url

parseInlineElement :: Parser InlineElement
parseInlineElement =
  choice
    [ parseImageText,
      parseLinkText,
      try parseCodeText,
      try parseBold,
      try parseItalic,
      parsePlainText
    ]

parseMdHeader :: Parser MarkdownElement
parseMdHeader = do
  level <- length <$> some (symbol '#')
  content <- many parseInlineElement
  _ <- (endOfLine *> pure ()) <|> eof
  return $ Header level content

parseParagraph :: Parser MarkdownElement
parseParagraph = do
  content <- some parseInlineElement
  _ <- endOfLine
  _ <- (endOfLine *> pure ()) <|> eof
  return $ Paragraph content

parseCodeBlock :: Parser MarkdownElement
parseCodeBlock = do
  _ <- string (Text.pack "```")
  lang <- optional $ takeWhileP (Just "language") (/= '\n')
  _ <- endOfLine
  content <-
    manyTill1
      anySingle
      ( try $ do
          _ <- endOfLine
          _ <- string (Text.pack "```")
          return ()
      )
  _ <- (endOfLine *> pure ()) <|> eof
  return $ CodeBlock (T.stripEnd <$> lang) (T.pack content)

parseBulletListInlineElem :: Parser [InlineElement]
parseBulletListInlineElem = do
  _ <- (char '*' <?> "* point") <|> (char '-' <?> "- point")
  _ <- char ' ' <?> "space"
  contentHead <- parseInlineElement
  contentTail <-
    many
      ( try $ do
          _ <- endOfLine
          parseInlineElement
      )
  return (contentHead : contentTail)

parseBulletList :: Parser MarkdownElement
parseBulletList = do
  level <- length <$> many (char ' ')
  firstItem <- parseBulletListInlineElem
  restItems <- many $ try $ do
    _ <- endOfLine
    indented level parseBulletListInlineElem
  return $ BulletList (firstItem : restItems)

parseNumberedListInlineElem :: Parser [InlineElement]
parseNumberedListInlineElem = do
  _ <- some (satisfy isDigit) <?> "digit"
  _ <- char '.' <?> "period"
  _ <- char ' ' <?> "space"

  contentHead <- parseInlineElement
  contentTail <-
    many
      ( try $ do
          _ <- endOfLine
          parseInlineElement
      )
  return (contentHead : contentTail)

parseNumberedList :: Parser MarkdownElement
parseNumberedList = do
  level <- length <$> many (char ' ')
  firstItem <- parseNumberedListInlineElem
  restItems <- many $ try $ do
    _ <- endOfLine
    indented level parseNumberedListInlineElem
  return $ NumberedList (firstItem : restItems)

parseHorizontalRule :: Parser MarkdownElement
parseHorizontalRule = HorizontalRule <$ try (symbol '-' >> symbol '-' >> symbol '-' >> endOfLine)

parseEmptyLine :: Parser MarkdownElement
parseEmptyLine = EmptyLine <$ endOfLine

-- | Parse markdown text into a MarkdownDoc structure
parseMarkdownElement :: Parser MarkdownElement
parseMarkdownElement =
  choice
    [ parseMdHeader,
      parseCodeBlock,
      try parseBulletList,
      try parseNumberedList,
      parseParagraph,
      parseHorizontalRule,
      parseEmptyLine
    ]