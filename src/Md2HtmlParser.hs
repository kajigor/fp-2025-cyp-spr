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
import Md2HtmlParser.Metrics
import Control.Monad.State.Strict
import qualified Data.Text as T
import Text.Megaparsec (runParserT)



processMarkdown :: T.Text -> (T.Text, Metrics)
processMarkdown markdown =
  let parserAction = runParserT parseMarkdownDoc "" markdown
      (result, finalMetrics) = runState parserAction emptyMetrics
  in case result of
       Left err -> (T.pack $ "Error parsing markdown:\n" ++ show err, finalMetrics)
       Right doc -> (renderHtml $ markdownToHtml doc, finalMetrics)
