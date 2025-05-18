{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module ListParsingSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Text.Megaparsec (parse, eof)
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils (Parser)
import Control.Monad.State.Strict (runState)
import Md2HtmlParser.Metrics (emptyMetrics)
import Text.Megaparsec (runParserT)

-- Helper to handle parsing results and throw proper test failures
parseMdOrFail :: Parser a -> T.Text -> IO a
parseMdOrFail parser input =
  let parserAction = runParserT (parser <* eof) "" input
      (result, _) = runState parserAction emptyMetrics
  in case result of
       Right x -> return x
       Left err -> expectationFailure ("Failed to parse: " ++ show err) >> undefined

-- Helper to check if parsing fails as expected
shouldFailToParse :: Show a => Parser a -> T.Text -> Expectation
shouldFailToParse parser input =
  let parserAction = runParserT parser "" input
      (result, _) = runState parserAction emptyMetrics
  in case result of
       Right result -> expectationFailure $
         "Expected parsing to fail, but succeeded with result: " ++ show result
       Left _ -> return () -- Error is expected, so test_

spec :: Spec
spec = do
  describe "Bullet List Parsing" $ do
    it "parses unordered lists with asterisks correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "* Item 1"
      result `shouldBe` BulletList [[PlainText "Item 1"]]
      
    it "parses unordered lists with hyphens correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "- Item 1"
      result `shouldBe` BulletList [[PlainText "Item 1"]]
      
    it "parses multiple bullet list items correctly" $ do
      let markdown = "* Item 1\n* Item 2\n* Item 3"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [BulletList items] -> 
          length items `shouldBe` 3
        _ -> expectationFailure $ "Expected BulletList with 3 items but got " ++ show result
      
    it "parses bullet lists with formatted text correctly" $ do
      let markdown = "* Item with *italic*\n* Item with **bold**\n* Item with `code`"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [BulletList items] -> do
          length items `shouldBe` 3
          length (items !! 0) `shouldBe` 2  -- PlainText + ItalicText
          length (items !! 1) `shouldBe` 2  -- PlainText + BoldText
          length (items !! 2) `shouldBe` 2  -- PlainText + CodeText
        _ -> expectationFailure $ "Expected BulletList with formatted text but got " ++ show result
      
    it "parses indented bullet lists correctly" $ do
      let markdown = "  * Indented item 1\n  * Indented item 2"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [BulletList items] -> 
          length items `shouldBe` 2
        _ -> expectationFailure $ "Expected BulletList with 2 items but got " ++ show result
        
  describe "Numbered List Parsing" $ do
    it "parses simple numbered lists correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "1. Item 1"
      result `shouldBe` NumberedList [[PlainText "Item 1"]]
      
    it "parses multiple numbered list items correctly" $ do
      let markdown = "1. Item 1\n2. Item 2\n3. Item 3"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [NumberedList items] -> 
          length items `shouldBe` 3
        _ -> expectationFailure $ "Expected NumberedList with 3 items but got " ++ show result
      
    it "parses numbered lists with non-sequential numbers correctly" $ do
      let markdown = "1. Item 1\n5. Item 5\n10. Item 10"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [NumberedList items] -> 
          length items `shouldBe` 3
        _ -> expectationFailure $ "Expected NumberedList with 3 items but got " ++ show result
      
    it "parses numbered lists with formatted text correctly" $ do
      let markdown = "1. Item with *italic*\n2. Item with **bold**\n3. Item with `code`"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [NumberedList items] -> do
          length items `shouldBe` 3
          length (items !! 0) `shouldBe` 2  -- PlainText + ItalicText
          length (items !! 1) `shouldBe` 2  -- PlainText + BoldText
          length (items !! 2) `shouldBe` 2  -- PlainText + CodeText
        _ -> expectationFailure $ "Expected NumberedList with formatted text but got " ++ show result
      
    it "parses indented numbered lists correctly" $ do
      let markdown = "  1. Indented item 1\n  2. Indented item 2"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [NumberedList items] -> 
          length items `shouldBe` 2
        _ -> expectationFailure $ "Expected NumberedList with 2 items but got " ++ show result
        
  describe "Mixed and Complex List Parsing" $ do
    it "parses mixed bullet point types correctly" $ do
      let markdown = "* Asterisk item\n- Hyphen item"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc [BulletList items] -> 
          length items `shouldBe` 2
        _ -> expectationFailure $ "Expected BulletList with 2 items but got " ++ show result
      
    it "parses lists with links correctly" $ do
      let markdown = "* Item with [link](url)"
      result <- parseMdOrFail parseMarkdownElement markdown
      case result of
        BulletList [[PlainText "Item with ", LinkText [PlainText "link"] "url"]] -> return ()
        _ -> expectationFailure $ "Expected BulletList with link but got " ++ show result
      
    it "parses lists with complex nested formatting correctly" $ do
      let markdown = "1. Item with *italic **bold** text*"
      result <- parseMdOrFail parseMarkdownElement markdown
      case result of
        NumberedList [[PlainText "Item with ", ItalicText elems]] -> 
          length elems `shouldBe` 3  -- PlainText + BoldText + PlainText
        _ -> expectationFailure $ "Expected NumberedList with complex formatting but got " ++ show result
        
  describe "List Error Cases" $ do
    it "fails for bullet lists without space after marker" $ do
      shouldFailToParse parseMarkdownElement "*No space"
      
    it "fails for numbered lists without space after marker" $ do
      result <- parseMdOrFail parseMarkdownElement "1.No space"
      case result of
        Paragraph [PlainText "1.No space"] -> return ()
        _ -> expectationFailure $ "Expected Paragraph but got " ++ show result
