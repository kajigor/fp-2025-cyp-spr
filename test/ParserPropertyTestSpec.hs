{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParserPropertyTestSpec (spec) where

import qualified Data.Text as T
import Md2HtmlParser.GeneratorUtil
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils
import Md2HtmlParser.Metrics (emptyMetrics)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Error (ParseErrorBundle, ShowErrorComponent (..))
import Control.Monad.State.Strict (runState)
import Md2HtmlParser.Metrics (Metrics(..))

instance ShowErrorComponent String where
  showErrorComponent = id
  errorComponentLen = length

testParser :: Parser a -> T.Text -> Either (ParseErrorBundle T.Text String) a
testParser p input =
  let parserAction = runParserT (p <* eof) "" input
      (result, _) = runState parserAction emptyMetrics
  in result

spec :: Spec
spec = do
  describe "parseInline" $ do
    it "parses valid inlineElement based on generated input and matches merged structure" $
      withMaxSuccess 1000 $
        property $
          forAll genInlineElement $ \expected -> do
            testParser parseInlineElement (renderInlineForTest expected) `shouldParse` expected

  describe "parseMdHeader" $ do
    it "parses valid headers based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genHeaderMarkdown $ \(mdInput, expectedHeader) ->
            testParser parseMdHeader mdInput `shouldParse` expectedHeader

  describe "parseParagraph" $ do
    it "parses valid paragraphs based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genParagraphMarkdown $ \(mdInput, expectedParagraph) ->
            testParser parseParagraph mdInput `shouldParse` expectedParagraph
    it "a paragraph can contain various generated inline elements" $
      -- Changed description
      withMaxSuccess 1000 $
        property $
          forAll (Paragraph <$> genInlineElementList) $ \paragraph ->
            let rendered = T.concat (map renderInlineForTest (case paragraph of Paragraph p -> p; _ -> [])) <> "\n"
             in testParser parseParagraph rendered `shouldParse` paragraph

  describe "Inline Element Parsers" $ do
    describe "parseBold" $
      it "parses bold text based on generated input" $
        -- Changed description
        withMaxSuccess 1000 $
          property $
            forAll genSpecificBold $ \(mdInput, expected) ->
              testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseItalic" $
      it "parses italic text based on generated input" $
        -- Changed description
        withMaxSuccess 1000 $
          property $
            forAll genSpecificItalic $ \(mdInput, expected) ->
              testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseCodeText" $
      it "parses inline code based on generated input" $
        -- Changed description
        property $
          forAll genSpecificCodeText $ \(mdInput, expected) ->
            testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseLinkText" $
      it "parses links based on generated input" $
        withMaxSuccess 1000 $
          property $
            forAll genSpecificLinkText $ \(mdInput, expected) ->
              testParser parseInlineElement mdInput `shouldParse` expected

    describe "parseImageText" $
      it "parses images based on generated input" $
        withMaxSuccess 1000 $
          property $
            forAll genSpecificImageText $ \(mdInput, expected) ->
              testParser parseInlineElement mdInput `shouldParse` expected

  describe "parseCodeBlock" $ do
    it "parses code blocks with and without language based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genCodeBlockMarkdown $ \(mdInput, expectedBlock) ->
            testParser parseCodeBlock mdInput `shouldParse` expectedBlock

  describe "parseBulletList" $ do
    it "parses simple bullet lists based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genBulletListMarkdown $ \(mdInput, expectedList) ->
            testParser parseBulletList mdInput `shouldParse` expectedList

  describe "parseNumberedList" $ do
    it "parses simple numbered lists based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genNumberedListMarkdown $ \(mdInput, expectedList) ->
            testParser parseNumberedList mdInput `shouldParse` expectedList

  describe "parseHorizontalRule" $ do
    it "parses horizontal rules based on generated input" $
      withMaxSuccess 1000 $
        property $
          forAll genHorizontalRuleMarkdown $ \(mdInput, expectedRule) ->
            testParser parseHorizontalRule mdInput `shouldParse` expectedRule

  describe "parseMarkdownDoc" $ do
    it "parses a document with multiple generated elements" $
      withMaxSuccess 1000 $
        property $
          forAll genMarkdownDoc $ \doc@(MarkdownDoc elements) ->
            let rendered = T.concat (map renderMarkdownElementForTest elements)
             in case testParser parseMarkdownDoc rendered of
                  Right (MarkdownDoc parsedElements) ->
                    counterexample ("Generated Doc: " ++ show doc ++ "\nParsed Doc: " ++ show (MarkdownDoc parsedElements) ++ "\nRendered Input:\n" ++ T.unpack rendered) $
                      parsedElements `shouldBe` elements
                  Left e -> counterexample (show e ++ "\nRendered Input:\n" ++ T.unpack rendered) False -- Added rendered input here too
    it "does not crash on arbitrary text input" $
      withMaxSuccess 1000 $
        property $
          \text -> case testParser parseMarkdownDoc (T.pack text) of
            Left _ -> property True
            Right _ -> property True

    describe "Metrics Profiling" $ do
      it "prints parser metrics on generated markdown" $
        withMaxSuccess 1 $  -- или больше, если хочешь
          property $
            forAll genMarkdownDoc $ \doc@(MarkdownDoc elements) ->
              let markdown = T.concat (map renderMarkdownElementForTest elements)
                  parserAction = runParserT parseMarkdownDoc "" markdown
                  (result, metrics) = runState parserAction emptyMetrics
              in case result of
                Right _ ->
                  counterexample
                    ("Parsed successfully!\n" ++
                     "Markdown size: " ++ show (T.length markdown) ++ "\n" ++
                     "Char calls: " ++ show (charCalls metrics) ++ "\n" ++
                     "Ratio (charCalls / size): " ++ show (fromIntegral (charCalls metrics) / fromIntegral (max 1 (T.length markdown)) :: Double) ++ "\n" ++
                     "Rendered Markdown:\n" ++ T.unpack markdown)
                    True
                Left e ->
                  counterexample
                    ("Parse error:\n" ++ show e ++ "\nInput:\n" ++ T.unpack markdown)
                    False

