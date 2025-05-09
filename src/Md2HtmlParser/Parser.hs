module Md2HtmlParser.Parser
  ( parseMarkdown,
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

import Data.Text (Text)
import qualified Data.Text as Text
import Md2HtmlParser.Parser.Utils (Parser, betweenChars, endOfLine, notSpecialChar, parens, square, symbol, takeRestOfLine, takeUntilSpecialOrNewline, textString)
import Text.Megaparsec (ParseErrorBundle, choice, many, takeWhileP, try, (<|>), some)
import Text.Megaparsec.Char (char)

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
  | CodeBlock Text
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
parsePlainText = do
  PlainText <$> takeUntilSpecialOrNewline

parseItalic :: Parser InlineElement
parseItalic = do
  start <- try (textString "_") <|> try (textString "*")
  content <- try (some parseInlineElement)
  _ <- (textString (Text.unpack start))
  return $ ItalicText content

parseBold :: Parser InlineElement
parseBold = do
  start <- try (textString "__") <|> try (textString "**")
  content <- try (some parseInlineElement)
  _ <- (textString (Text.unpack start))
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

-- | Parse markdown text into a MarkdownDoc structure
parseMarkdown :: Text -> Either (ParseErrorBundle Text String) MarkdownDoc
parseMarkdown = undefined