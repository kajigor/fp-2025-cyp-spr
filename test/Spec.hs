{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import Md2HtmlParser
import Md2HtmlParser.Parser
import Md2HtmlParser.HTML

main :: IO ()
main = hspec $ do
  describe "Markdown Parser" $ do
    it "parses headers correctly" $ do
      let result = parseMarkdown "# Header 1"
      case result of
        Right (MarkdownDoc [Header 1 [PlainText "Header 1"]]) -> return ()
        _ -> expectationFailure $ "Failed to parse header: " ++ show result

    it "parses paragraphs correctly" $ do
      let result = parseMarkdown "This is a paragraph."
      case result of
        Right (MarkdownDoc [Paragraph [PlainText "This is a paragraph."]]) -> return ()
        _ -> expectationFailure $ "Failed to parse paragraph: " ++ show result

    it "parses bullet lists correctly" $ do
      let result = parseMarkdown "* Item 1\n* Item 2"
      case result of
        Right (MarkdownDoc [BulletList items]) -> length items `shouldBe` 2
        _ -> expectationFailure $ "Failed to parse bullet list: " ++ show result

    it "parses code blocks correctly" $ do
      let result = parseMarkdown "```\ncode block\n```"
      case result of
        Right (MarkdownDoc [CodeBlock code]) -> code `shouldBe` "code block\n"
        _ -> expectationFailure $ "Failed to parse code block: " ++ show result

  describe "HTML Conversion" $ do
    it "converts headers to HTML" $ do
      let md = MarkdownDoc [Header 1 [PlainText "Header 1"]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf "<h1>Header 1</h1>" html `shouldBe` True

    it "converts paragraphs to HTML" $ do
      let md = MarkdownDoc [Paragraph [PlainText "This is a paragraph."]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf "<p>This is a paragraph.</p>" html `shouldBe` True

    it "converts bullet lists to HTML" $ do
      let md = MarkdownDoc [BulletList [[PlainText "Item 1"], [PlainText "Item 2"]]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf "<ul>" html `shouldBe` True
      T.isInfixOf "<li>Item 1</li>" html `shouldBe` True
      T.isInfixOf "<li>Item 2</li>" html `shouldBe` True

    it "converts code blocks to HTML" $ do
      let md = MarkdownDoc [CodeBlock "code block"]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf "<pre><code>code block</code></pre>" html `shouldBe` True
import Test.QuickCheck
import Text.Parsec (parse)

import Md2HtmlParser
import Md2HtmlParser.Parser
import Md2HtmlParser.HTML

main :: IO ()
main = hspec $ do
  describe "Markdown Parser" $ do
    it "parses headers correctly" $ do
      let result = parseMarkdown "# Header 1"
      result `shouldBe` Right (MarkdownDoc [Header 1 "Header 1"])
    
    it "parses paragraphs correctly" $ do
      let result = parseMarkdown "This is a paragraph."
      result `shouldBe` Right (MarkdownDoc [Paragraph "This is a paragraph."])
    
    it "parses multiple elements" $ do
      let md = "# Header\n\nThis is a paragraph."
      let expected = Right (MarkdownDoc [Header 1 "Header", Paragraph "This is a paragraph."])
      parseMarkdown md `shouldBe` expected
      
  describe "HTML Generation" $ do
    it "converts headers to HTML" $ do
      let mdDoc = MarkdownDoc [Header 1 "Title"]
      let htmlDoc = markdownToHtml mdDoc
      let rendered = renderHtml htmlDoc
      rendered `shouldContain` "<h1>Title</h1>"
    
    it "converts paragraphs to HTML" $ do
      let mdDoc = MarkdownDoc [Paragraph "Test paragraph"]
      let htmlDoc = markdownToHtml mdDoc
      let rendered = renderHtml htmlDoc
      rendered `shouldContain` "<p>Test paragraph</p>"