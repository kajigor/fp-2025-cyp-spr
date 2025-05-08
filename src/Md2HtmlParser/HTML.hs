{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser.HTML 
  ( markdownToHtml
  , renderHtml
  , HtmlDoc(..)
  , HtmlElement(..)
  , HtmlAttribute(..)
  ) where

import Md2HtmlParser.Parser (MarkdownDoc(..), MarkdownElement(..), InlineElement(..))
import qualified Data.Text as T
import Data.Text (Text)

-- | Represents an HTML document
newtype HtmlDoc = HtmlDoc [HtmlElement]
  deriving (Show, Eq)

-- | Different HTML elements
data HtmlElement
  = HtmlTag Text [HtmlAttribute] [HtmlElement]  -- ^ HTML tag with attributes and children
  | HtmlText Text                               -- ^ Plain text content
  deriving (Show, Eq)

-- | HTML attribute name-value pair
data HtmlAttribute = HtmlAttribute Text Text
  deriving (Show, Eq)

-- | Convert a markdown document to HTML
markdownToHtml :: MarkdownDoc -> HtmlDoc
markdownToHtml = undefined

-- | Render HTML document to Text
renderHtml :: HtmlDoc -> Text
renderHtml = undefined

-- | Convert a markdown element to HTML
convertElement :: MarkdownElement -> HtmlElement
convertElement = undefined

-- | Convert inline markdown elements to HTML
convertInline :: InlineElement -> HtmlElement
convertInline = undefined

-- | Render an HTML element to string
renderElement :: HtmlElement -> Text
renderElement = undefined

-- | Render HTML attributes
renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = undefined

-- | Render a single HTML attribute
renderAttribute :: HtmlAttribute -> Text
renderAttribute = undefined

