{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser
  ( parseMarkdownElement
  , markdownToHtml
  , renderHtml
  , processMarkdown
  , MarkdownDoc(..)
  , HtmlDoc(..)
  ) where

import Md2HtmlParser.Parser (parseMarkdownElement, parseMarkdownDoc, MarkdownDoc(..))
import Md2HtmlParser.HTML (markdownToHtml, renderHtml, HtmlDoc(..))
import qualified Data.Text as T
import qualified Text.Megaparsec

-- | Process markdown text to HTML
processMarkdown :: T.Text -> T.Text
processMarkdown markdown = 
  case Text.Megaparsec.parse parseMarkdownDoc "" markdown of
    Left err -> T.pack $ "Error parsing markdown: " ++ show err
    Right doc -> renderHtml $ markdownToHtml doc

