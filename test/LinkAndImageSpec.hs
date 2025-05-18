{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module LinkAndImageSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Text.Megaparsec (parse, eof)
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils (Parser)
import Text.Megaparsec (runParserT, eof)
import Control.Monad.State.Strict (runState)
import Md2HtmlParser.Metrics (emptyMetrics)

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
       Left _ -> return ()

spec :: Spec
spec = do
  describe "Link Parsing" $ do
    it "parses simple links correctly" $ do
      result <- parseMdOrFail parseInlineElement "[link text](http://example.com)"
      result `shouldBe` LinkText [PlainText "link text"] "http://example.com"
      
    it "parses links with query parameters correctly" $ do
      result <- parseMdOrFail parseInlineElement "[search](https://example.com?q=test&p=1)"
      result `shouldBe` LinkText [PlainText "search"] "https://example.com?q=test&p=1"
      
    it "parses links with formatted text correctly" $ do
      result <- parseMdOrFail parseInlineElement "[*italic* and **bold**](url)"
      case result of
        LinkText elements url -> do
          length elements `shouldBe` 3
          elements !! 0 `shouldBe` ItalicText [PlainText "italic"]
          elements !! 1 `shouldBe` PlainText " and "
          elements !! 2 `shouldBe` BoldText [PlainText "bold"]
          url `shouldBe` "url"
        _ -> expectationFailure $ "Expected LinkText but got " ++ show result
        
    it "parses relative links correctly" $ do
      result <- parseMdOrFail parseInlineElement "[relative link](../folder/file.html)"
      result `shouldBe` LinkText [PlainText "relative link"] "../folder/file.html"
      
    it "parses links with nested formatting correctly" $ do
      result <- parseMdOrFail parseInlineElement "[link with *nested **formatting***](url)"
      case result of
        LinkText [PlainText "link with ", ItalicText elems] url ->
          elems !! 1 `shouldBe` BoldText [PlainText "formatting"]
        _ -> expectationFailure $ "Expected LinkText with nested formatting but got " ++ show result
        
  describe "Image Parsing" $ do
    it "parses simple images correctly" $ do
      result <- parseMdOrFail parseInlineElement "![alt text](image.jpg)"
      result `shouldBe` ImageText "alt text" "image.jpg"
      
    it "parses images with paths correctly" $ do
      result <- parseMdOrFail parseInlineElement "![logo](/images/logo.png)"
      result `shouldBe` ImageText "logo" "/images/logo.png"
      
    it "parses images with URLs correctly" $ do
      result <- parseMdOrFail parseInlineElement "![remote image](https://example.com/img.jpg)"
      result `shouldBe` ImageText "remote image" "https://example.com/img.jpg"
      
    it "parses images with empty alt text correctly" $ do
      result <- parseMdOrFail parseInlineElement "![](image.jpg)"
      result `shouldBe` ImageText "" "image.jpg"
      
  describe "Link and Image Error Cases" $ do
    it "fails for unclosed link text" $ do
      shouldFailToParse parseInlineElement "[unclosed link"
      
    it "fails for link with missing URL" $ do
      shouldFailToParse parseInlineElement "[link text]()"
      
    it "fails for image with missing URL" $ do
      shouldFailToParse parseInlineElement "![alt text]()"
      
    it "fails for unclosed image text" $ do
      shouldFailToParse parseInlineElement "![unclosed image"
