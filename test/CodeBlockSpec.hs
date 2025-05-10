{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module CodeBlockSpec (spec) where

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

spec :: Spec
spec = do
  describe "Code Block Parsing" $ do
    it "parses simple code blocks correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```\ncode block\n```"
      result `shouldBe` CodeBlock Nothing "code block"
      
    it "parses code blocks with language correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```haskell\nfoo :: Int -> Int\nfoo x = x + 1\n```"
      result `shouldBe` CodeBlock (Just "haskell") "foo :: Int -> Int\nfoo x = x + 1"
      
    it "parses empty code blocks correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```\n\n```"
      result `shouldBe` CodeBlock Nothing ""
      
    it "parses code blocks with multiple lines correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```\nline 1\nline 2\nline 3\n```"
      result `shouldBe` CodeBlock Nothing "line 1\nline 2\nline 3"
      
    it "parses code blocks with special characters correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```\nfunc() { return *ptr; }\n```"
      result `shouldBe` CodeBlock Nothing "func() { return *ptr; }"
      
    it "parses code blocks with indentation correctly" $ do
      result <- parseMdOrFail parseMarkdownElement "```\n    indented code\n```"
      result `shouldBe` CodeBlock Nothing "    indented code"
      
    it "parses code blocks with various language specifiers correctly" $ do
      result1 <- parseMdOrFail parseMarkdownElement "```python\nprint('Hello')\n```"
      result1 `shouldBe` CodeBlock (Just "python") "print('Hello')"
      
      result2 <- parseMdOrFail parseMarkdownElement "```javascript\nconsole.log('Hello');\n```"
      result2 `shouldBe` CodeBlock (Just "javascript") "console.log('Hello');"
      
      result3 <- parseMdOrFail parseMarkdownElement "```css\nbody { margin: 0; }\n```"
      result3 `shouldBe` CodeBlock (Just "css") "body { margin: 0; }"
      
  describe "Code Block Error Cases" $ do
    it "fails for unclosed code blocks" $ do
      shouldFailToParse parseMarkdownElement "```\nunclosed code block"
      
    it "fails for code blocks without ending newline" $ do
      shouldFailToParse parseMarkdownElement "```\ncode```"
      
    it "fails for improperly formatted language specifier" $ do
      shouldFailToParse parseMarkdownElement "``` invalid language\ncode\n```"
