{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module InlineElementSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Text.Megaparsec (parse, errorBundlePretty, eof)
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils (Parser)
import Data.Void (Void)

-- Helper to handle parsing results and throw proper test failures
parseMdOrFail :: Parser a -> T.Text -> IO a
parseMdOrFail parser input =
  case parse (parser <* eof) "" input of
    Right result -> return result
    Left err -> expectationFailure ("Failed to parse: " ++ show err) >> undefined

-- Helper to check if parsing fails as expected
shouldFailToParse :: Show a => Parser a -> T.Text -> Expectation
shouldFailToParse parser input =
  case parse parser "" input of
    Right result -> expectationFailure $ 
      "Expected parsing to fail, but succeeded with result: " ++ show result
    Left _ -> return () -- Error is expected, so test passes

shouldParsePartially :: Show a => Parser a -> T.Text -> Expectation
shouldParsePartially parser input =
  case parse parser "" input of
    Right _ -> do
      case parse (parser <* eof) "" input of
        Right _ -> expectationFailure "Expected partial parsing, but succeeded to parse the whole input"
        Left _ -> return ()
    Left err -> expectationFailure $
      "Expected partial parsing, but failed to parse anything: " ++ show err


spec :: Spec
spec = do
  describe "Inline Element Parsing" $ do
    it "parses plain text correctly" $ do
      result <- parseMdOrFail parseInlineElement "simple text"
      result `shouldBe` PlainText "simple text"
      
    it "parses italic text with asterisks correctly" $ do
      result <- parseMdOrFail parseInlineElement "*italic text*"
      result `shouldBe` ItalicText [PlainText "italic text"]
      
    it "parses italic text with underscores correctly" $ do
      result <- parseMdOrFail parseInlineElement "_italic text_"
      result `shouldBe` ItalicText [PlainText "italic text"]
      
    it "parses bold text with double asterisks correctly" $ do
      result <- parseMdOrFail parseInlineElement "**bold text**"
      result `shouldBe` BoldText [PlainText "bold text"]
      
    it "parses bold text with double underscores correctly" $ do
      result <- parseMdOrFail parseInlineElement "__bold text__"
      result `shouldBe` BoldText [PlainText "bold text"]

    it "parses bold italic text correctly" $ do
      result1 <- parseMdOrFail parseInlineElement "___bold italic text___"
      result2 <- parseMdOrFail parseInlineElement "__*bold italic text*__"
      result3 <- parseMdOrFail parseInlineElement "**_bold italic text_**"
      result4 <- parseMdOrFail parseInlineElement "***bold italic text***"
      result1 `shouldBe` BoldText [ItalicText [PlainText "bold italic text"]]
      result2 `shouldBe` BoldText [ItalicText [PlainText "bold italic text"]]
      result3 `shouldBe` BoldText [ItalicText [PlainText "bold italic text"]]
      result4 `shouldBe` BoldText [ItalicText [PlainText "bold italic text"]]

    it "parses italic bold text correctly" $ do
      result1 <- parseMdOrFail parseInlineElement "*__bold italic text__*"
      result2 <- parseMdOrFail parseInlineElement "_**bold italic text**_"
      result1 `shouldBe` ItalicText [BoldText [PlainText "bold italic text"]]
      result2 `shouldBe` ItalicText [BoldText [PlainText "bold italic text"]]

    it "parses code text correctly" $ do
      result <- parseMdOrFail parseInlineElement "`code text`"
      result `shouldBe` CodeText "code text"
      
    it "parses link text correctly" $ do
      result <- parseMdOrFail parseInlineElement "[link text](http://example.com)"
      result `shouldBe` LinkText [PlainText "link text"] "http://example.com"
      
    it "parses image text correctly" $ do
      result <- parseMdOrFail parseInlineElement "![alt text](image.jpg)"
      result `shouldBe` ImageText "alt text" "image.jpg"
    
  describe "Nested Formatting" $ do
    it "parses italic with bold inside" $ do
      result <- parseMdOrFail parseInlineElement "*italic with **bold** inside*"
      case result of
        ItalicText elements -> do
          length elements `shouldBe` 3
          elements !! 0 `shouldBe` PlainText "italic with "
          elements !! 1 `shouldBe` BoldText [PlainText "bold"]
          elements !! 2 `shouldBe` PlainText " inside"
        _ -> expectationFailure $ "Expected ItalicText but got " ++ show result
      
    it "parses bold with italic inside" $ do
      result <- parseMdOrFail parseInlineElement "**bold with _italic_ inside**"
      case result of
        BoldText elements -> do
          length elements `shouldBe` 3
          elements !! 0 `shouldBe` PlainText "bold with "
          elements !! 1 `shouldBe` ItalicText [PlainText "italic"]
          elements !! 2 `shouldBe` PlainText " inside"
        _ -> expectationFailure $ "Expected BoldText but got " ++ show result
      
    it "parses links with formatted text inside" $ do
      result <- parseMdOrFail parseInlineElement "[Link with *italic* text](url)"
      case result of
        LinkText elements url -> do
          length elements `shouldBe` 3
          elements !! 0 `shouldBe` PlainText "Link with "
          elements !! 1 `shouldBe` ItalicText [PlainText "italic"]
          elements !! 2 `shouldBe` PlainText " text"
          url `shouldBe` "url"
        _ -> expectationFailure $ "Expected LinkText but got " ++ show result

  describe "Failed Parsing Cases" $ do
    it "fails for unclosed italic" $ do
      shouldFailToParse parseInlineElement "*unclosed italic"
      
    it "fails for unclosed bold" $ do
      shouldFailToParse parseInlineElement "**unclosed bold"
      
    it "fails for empty italic markers" $ do
      shouldFailToParse parseInlineElement "**"
      
    it "fails for empty bold content" $ do
      shouldFailToParse parseInlineElement "****"
      
    it "fails for mismatched italic markers" $ do
      shouldFailToParse parseInlineElement "*italic_"
      
    it "fails for mismatched bold markers" $ do
      shouldFailToParse parseInlineElement "**bold__"
      
    it "fails for unclosed code" $ do
      shouldFailToParse parseInlineElement "`unclosed code"
      
    it "fails for unclosed link" $ do
      shouldFailToParse parseInlineElement "[link text"
      
    it "fails for link without URL" $ do
      shouldFailToParse parseInlineElement "[link text]"
      
    it "fails for unclosed image" $ do
      shouldFailToParse parseInlineElement "![alt text"

    it "fails for invalid nested structure" $ do
      shouldParsePartially parseInlineElement "*outer **inner*"

    it "fails for too mane line breakes" $ do
      shouldParsePartially parseInlineElement "alt text\n"
