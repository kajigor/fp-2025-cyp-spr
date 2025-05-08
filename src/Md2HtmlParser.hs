{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser 
  ( parseMarkdown
  , markdownToHtml
  , renderHtml
  , processMarkdown
  , MarkdownDoc(..)
  , HtmlDoc(..)
  ) where

import Md2HtmlParser.Parser (parseMarkdown, MarkdownDoc(..))
import Md2HtmlParser.HTML (markdownToHtml, renderHtml, HtmlDoc(..))
import qualified Data.Text as T

-- | Process markdown text to HTML
processMarkdown :: T.Text -> T.Text
processMarkdown = undefined

