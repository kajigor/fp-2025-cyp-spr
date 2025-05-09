{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MarkdownDocSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Text.Megaparsec (parse, eof)
import Md2HtmlParser.Parser
import Md2HtmlParser.Parser.Utils (Parser)

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

shouldParsePartially :: Parser a -> T.Text -> Expectation
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
  describe "Markdown Element Parsing" $ do
    it "parses header correctly" $ do
      result1 <- parseMdOrFail parseMarkdownElement "# Level 1 Header"
      result1 `shouldBe` Header 1 [PlainText "Level 1 Header"]
      
      result2 <- parseMdOrFail parseMarkdownElement "## Level 2 Header"
      result2 `shouldBe` Header 2 [PlainText "Level 2 Header"]
      
      result3 <- parseMdOrFail parseMarkdownElement "### Level 3 Header"
      result3 `shouldBe` Header 3 [PlainText "Level 3 Header"]
      
    it "parses headers with formatted text" $ do
      result <- parseMdOrFail parseMarkdownElement "# Header with *italic* and **bold**"
      case result of
        Header level elements -> do
          level `shouldBe` 1
          length elements `shouldBe` 4
          elements !! 0 `shouldBe` PlainText "Header with "
          elements !! 1 `shouldBe` ItalicText [PlainText "italic"]
          elements !! 2 `shouldBe` PlainText " and "
          elements !! 3 `shouldBe` BoldText [PlainText "bold"]
        _ -> expectationFailure $ "Expected Header but got " ++ show result
      
    it "parses paragraphs correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "This is a simple paragraph"
      result `shouldBe` Paragraph [PlainText "This is a simple paragraph"]
      
    it "parses paragraphs with formatted text" $ do
      result <- parseMdOrFail parseMarkdownElement "Paragraph with *italic* and **bold** text"
      case result of
        Paragraph elements -> do
          length elements `shouldBe` 5
          elements !! 0 `shouldBe` PlainText "Paragraph with "
          elements !! 1 `shouldBe` ItalicText [PlainText "italic"]
          elements !! 2 `shouldBe` PlainText " and "
          elements !! 3 `shouldBe` BoldText [PlainText "bold"]
          elements !! 4 `shouldBe` PlainText " text"
        _ -> expectationFailure $ "Expected Paragraph but got " ++ show result
      
    it "parses code blocks correctly" $ do
      result1 <- parseMdOrFail parseMarkdownElement "```\ncode block\n```"
      result1 `shouldBe` CodeBlock Nothing "code block"
      
      result2 <- parseMdOrFail parseMarkdownElement "```haskell\nfoo :: Int -> Int\nfoo x = x + 1\n```"
      result2 `shouldBe` CodeBlock (Just "haskell") "foo :: Int -> Int\nfoo x = x + 1"
      
    it "parses bullet lists correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "* Item 1"
      result `shouldBe` BulletList [[PlainText "Item 1"]]
      
      result2 <- parseMdOrFail parseMarkdownElement "- Item 1"
      result2 `shouldBe` BulletList [[PlainText "Item 1"]]
      
    it "parses numbered lists correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "1. Item 1"
      result `shouldBe` NumberedList [[PlainText "Item 1"]]
      
    it "parses horizontal rule correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "---"
      result `shouldBe` HorizontalRule
      
    it "parses empty line correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "\n"
      result `shouldBe` EmptyLine
        
  describe "Markdown Document Parsing" $ do
    it "parses empty document correctly" $ do
      result <- parseMdOrFail parseMarkdownDoc ""
      result `shouldBe` MarkdownDoc []
      
    it "parses document with single element" $ do
      result <- parseMdOrFail parseMarkdownDoc "# Header"
      result `shouldBe` MarkdownDoc [Header 1 [PlainText "Header"]]
      
    it "parses document with multiple elements" $ do
      let markdown = "# Header\n\nThis is a paragraph\n\n```\ncode\n```\n"
      result <- parseMdOrFail parseMarkdownDoc markdown
      result `shouldBe` MarkdownDoc 
        [ Header 1 [PlainText "Header"]
        , EmptyLine
        , Paragraph [PlainText "This is a paragraph"]
        , EmptyLine
        , CodeBlock Nothing "code"
        ]
        
    it "parses mixed elements correctly" $ do
      let markdown = "# Header\n\n  1. First item\n  2. Second item\n\n  * Bullet item\n\n---\n"
      result <- parseMdOrFail parseMarkdownDoc markdown
      case result of
        MarkdownDoc elements -> do
          length elements `shouldBe` 7
          elements !! 0 `shouldBe` Header 1 [PlainText "Header"]
          elements !! 1 `shouldBe` EmptyLine
          case elements !! 2 of
            NumberedList items -> do
              length items `shouldBe` 2
              items !! 0 `shouldBe` [PlainText "First item"]
              items !! 1 `shouldBe` [PlainText "Second item"]
            _ -> expectationFailure $ "Expected NumberedList but got " ++ show (elements !! 2)
          elements !! 3 `shouldBe` EmptyLine
          elements !! 4 `shouldBe` BulletList [[PlainText "Bullet item"]]
          elements !! 5 `shouldBe` EmptyLine
          elements !! 6 `shouldBe` HorizontalRule
        _ -> expectationFailure $ "Expected MarkdownDoc but got " ++ show result
        
  describe "Failed Parsing Cases" $ do
    it "fails for unclosed code block" $ do
      shouldFailToParse parseMarkdownElement "```\nunclosed code block"
      
    it "fails for malformed bullet list" $ do
      shouldFailToParse parseMarkdownElement "*malformed"
      
    it "fails for malformed numbered list" $ do
      result <- parseMdOrFail parseMarkdownElement "1.malformed"
      result `shouldBe` Paragraph [PlainText "1.malformed"]
