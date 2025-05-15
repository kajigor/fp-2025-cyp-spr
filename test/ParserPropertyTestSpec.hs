{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserPropertyTestSpec (spec) where

import qualified Data.Text as T
import GeneratorUtil
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent (..))

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length

testParser :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text String) a
testParser p = parse (p <* eof) ""

spec :: Spec
spec = do
  describe "parseMdHeader" $ do
    it "parses valid headers" $
      property $
        forAll genHeaderMarkdown $ \(mdInput, expectedHeader) ->
          testParser parseMdHeader mdInput `shouldParse` expectedHeader

    it "does not parse text without # as a header" $
      (testParser parseMdHeader `shouldFailOn` "just text\n")

    it "parses a header with trailing spaces before newline" $
      testParser parseMdHeader "# Header   \n" `shouldParse` Header 1 [PlainText "Header"]

  describe "parseParagraph" $ do
    it "parses valid paragraphs" $
      property $
        forAll genParagraphMarkdown $ \(mdInput, expectedParagraph) ->
          testParser parseParagraph mdInput `shouldParse` expectedParagraph
    it "a paragraph can contain various inline elements" $
      property $
        forAll (Paragraph <$> genInlineElementList) $ \paragraph ->
          let rendered = T.concat (map renderInlineForTest (case paragraph of Paragraph p -> p; _ -> [])) <> "\n"
           in testParser parseParagraph rendered `shouldParse` paragraph

  describe "Inline Element Parsers" $ do
    describe "parseBold" $
      it "parses bold text" $
        property $
          forAll genSpecificBold $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseItalic" $
      it "parses italic text" $
        property $
          forAll genSpecificItalic $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseCodeText" $
      it "parses inline code" $
        property $
          forAll genSpecificCodeText $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseLinkText" $
      it "parses links" $
        property $
          forAll genSpecificLinkText $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseImageText" $
      it "parses images" $
        property $
          forAll genSpecificImageText $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parsePlainText" $ do
      it "parses plain text" $
        testParser parseInlineElement "plain text" `shouldParse` PlainText "plain text"
      it "parses text up to a special character" $
        testParser parseInlineElement "text *italic*" `shouldParse` PlainText "text "

  describe "parseCodeBlock" $ do
    it "parses code blocks with and without language" $
      property $
        forAll genCodeBlockMarkdown $ \(mdInput, expectedBlock) ->
          testParser parseCodeBlock mdInput `shouldParse` expectedBlock
    it "parses a code block without a language" $
      testParser parseCodeBlock "```\ncode\n```\n" `shouldParse` CodeBlock Nothing "code\n"
    it "parses a code block with a language" $
      testParser parseCodeBlock "```haskell\ncode\n\n```\n" `shouldParse` CodeBlock (Just "haskell") "code\n"
    it "parses a code block with empty lines" $
      testParser parseCodeBlock "```\nline1\n\nline2\n```\n" `shouldParse` CodeBlock Nothing "line1\n\nline2\n"

  describe "parseBulletList" $ do
    it "parses simple bullet lists" $
      property $
        forAll genBulletListMarkdown $ \(mdInput, expectedList) ->
          testParser parseBulletList mdInput `shouldParse` expectedList
    it "parses a list with a single item" $
      testParser parseBulletList "* item\n" `shouldParse` BulletList [[PlainText "item"]]

  describe "parseNumberedList" $ do
    it "parses simple numbered lists" $
      property $
        forAll genNumberedListMarkdown $ \(mdInput, expectedList) ->
          testParser parseNumberedList mdInput `shouldParse` expectedList
    it "parses a list with a single item" $
      testParser parseNumberedList "1. item\n" `shouldParse` NumberedList [[PlainText "item"]]

  describe "parseHorizontalRule" $ do
    it "parses horizontal rules" $
      property $
        forAll genHorizontalRuleMarkdown $ \(mdInput, expectedRule) ->
          testParser parseHorizontalRule mdInput `shouldParse` expectedRule
    it "parses '---' as a horizontal rule" $
      testParser parseHorizontalRule "---\n" `shouldParse` HorizontalRule

  describe "parseMarkdownDoc" $ do
    it "parses an empty document" $
      testParser parseMarkdownDoc "" `shouldParse` MarkdownDoc []

    it "parses a document with multiple elements" $
      property $
        forAll genMarkdownDoc $ \doc@(MarkdownDoc elements) ->
          let rendered = T.concat (map renderMarkdownElementForTest elements)
           in case testParser parseMarkdownDoc rendered of
                Right (MarkdownDoc parsedElements) -> length elements === length parsedElements
                Left e -> counterexample (show e ++ "\nInput was:\n" ++ T.unpack rendered) False

    it "does not crash on arbitrary text input" $
      property $
        \text -> case testParser parseMarkdownDoc (T.pack text) of
          Left _ -> property True
          Right _ -> property True

renderMarkdownElementForTest :: MarkdownElement -> T.Text
renderMarkdownElementForTest (Header lvl inlines) = T.replicate lvl "#" <> " " <> T.concat (map renderInlineForTest inlines) <> "\n"
renderMarkdownElementForTest (Paragraph inlines) = T.concat (map renderInlineForTest inlines) <> "\n"
renderMarkdownElementForTest (CodeBlock lang code) = "```" <> maybe "" (<> "\n") lang <> code <> "\n```\n"
renderMarkdownElementForTest (BulletList items) = T.unlines (map (("* " <>) . T.concat . map renderInlineForTest) items) <> "\n"
renderMarkdownElementForTest (NumberedList items) = T.unlines (zipWith (\i item -> T.pack (show i) <> ". " <> T.concat (map renderInlineForTest item)) [1 :: Int ..] items) <> "\n"
renderMarkdownElementForTest HorizontalRule = "---\n"
renderMarkdownElementForTest EmptyLine = "\n"
renderMarkdownElementForTest (BlockQuote _els) = "> TODO BlockQuote render for test\n"