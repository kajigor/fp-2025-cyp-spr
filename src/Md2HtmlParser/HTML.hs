{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser.HTML
  ( markdownToHtml
  , renderHtml
  , HtmlDoc(..)
  , HtmlElement(..)
  , HtmlAttribute(..)
  , renderAttributes
  , renderElement
  ) where

import Md2HtmlParser.Parser (MarkdownDoc(..), MarkdownElement(..), InlineElement(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Represents an HTML document
newtype HtmlDoc = HtmlDoc [HtmlElement]
  deriving (Show, Eq)

-- | Different HTML elements
data HtmlElement
  = HtmlTag Text [HtmlAttribute] [HtmlElement]  -- tag, attrs, children
  | HtmlText Text                               -- text node
  deriving (Show, Eq)

-- | HTML attribute name-value pair
data HtmlAttribute = HtmlAttribute Text Text
  deriving (Show, Eq)

-- | Convert a markdown document to HTML
markdownToHtml :: MarkdownDoc -> HtmlDoc
markdownToHtml (MarkdownDoc elems) = HtmlDoc (map convertElement elems)

-- | Render HTML document to Text
renderHtml :: HtmlDoc -> Text
renderHtml (HtmlDoc elems) =
  "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n</head>\n<body>\n" <>
  T.concat (map renderElement elems) <>
  "\n</body>\n</html>"

-- | Convert a markdown element to HTML
convertElement :: MarkdownElement -> HtmlElement
convertElement el = case el of
  Header lvl inls        -> HtmlTag (T.pack ("h" ++ show lvl)) [] (map convertInline inls)
  Paragraph inls         -> HtmlTag "p" [] (map convertInline inls)
  CodeBlock mlang code   ->
    let attrs = case mlang of
                  Just lang -> [HtmlAttribute "class" ("language-" <> lang)]
                  Nothing   -> []
    in HtmlTag "pre" [] [HtmlTag "code" attrs [HtmlText code]]
  BulletList items       -> HtmlTag "ul" [] (map (\inls -> HtmlTag "li" [] (map convertInline inls)) items)
  NumberedList items     -> HtmlTag "ol" [] (map (\inls -> HtmlTag "li" [] (map convertInline inls)) items)
  HorizontalRule         -> HtmlTag "hr" [] []
  EmptyLine              -> HtmlTag "br" [] []
  BlockQuote els         -> HtmlTag "blockquote" [] (map convertElement els)

-- | Convert inline markdown to HTML
convertInline :: InlineElement -> HtmlElement
convertInline inl = case inl of
  PlainText t           -> HtmlText t
  ItalicText inls       -> HtmlTag "em" [] (map convertInline inls)
  BoldText inls         -> HtmlTag "strong" [] (map convertInline inls)
  CodeText code         -> HtmlTag "code" [] [HtmlText code]
  LinkText inls url     -> HtmlTag "a" [HtmlAttribute "href" url] (map convertInline inls)
  ImageText alt url     -> HtmlTag "img" [HtmlAttribute "src" url, HtmlAttribute "alt" alt] []

-- | Escape text for HTML
escapeText :: Text -> Text
escapeText = T.concatMap esc
  where
    esc '&'   = "&amp;"
    esc '<'   = "&lt;"
    esc '>'   = "&gt;"
    esc '"'  = "&quot;"
    esc '\'' = "&#39;"
    esc c     = T.singleton c

-- | Render HTML attributes
renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = T.concat . map renderAttribute

-- | Render a single attribute
renderAttribute :: HtmlAttribute -> Text
renderAttribute (HtmlAttribute name val) =
  " " <> name <> "=\"" <> escapeText val <> "\""

-- | Helpers for tags and children
openTag :: Text -> [HtmlAttribute] -> Text
openTag name attrs = "<" <> name <> renderAttributes attrs <> ">"

closeTag :: Text -> Text
closeTag name = "</" <> name <> ">"

renderChildren :: [HtmlElement] -> Text
renderChildren = T.concat . map renderElement

-- | Render an element to Text
renderElement :: HtmlElement -> Text
renderElement el = case el of
  HtmlText t -> escapeText t
  HtmlTag "br" _ _ -> "<br>"
  HtmlTag "hr" _ _ -> "<hr>"
  HtmlTag tag attrs []
    | (tag `elem` ["img","input","br","hr","meta","link"]) -> (openTag tag attrs)
    | otherwise                                                -> openTag tag attrs <> closeTag tag
  HtmlTag tag attrs ch                                         -> openTag tag attrs <> renderChildren ch <> closeTag tag
