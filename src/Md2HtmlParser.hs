{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser 
  ( parseMarkdownElement
  , markdownToHtml
  , renderHtml
  , processMarkdown
  , MarkdownDoc(..)
  , HtmlDoc(..)
  ) where

import Md2HtmlParser.Parser (parseMarkdownElement, MarkdownDoc(..))
import Md2HtmlParser.HTML (markdownToHtml, renderHtml, HtmlDoc(..))
import qualified Data.Text as T

-- | Process markdown text to HTML
processMarkdown :: T.Text -> T.Text
processMarkdown = undefined

