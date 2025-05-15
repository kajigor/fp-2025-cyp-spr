{-# LANGUAGE OverloadedStrings #-}
module GeneratorUtil where

import Test.QuickCheck
import qualified Data.Text as T
import Md2HtmlParser.Parser (InlineElement(..), MarkdownElement(..), MarkdownDoc(..))
import Control.Monad (replicateM)

-- | Generates safe text (without special Markdown characters)
genSafeText :: Gen T.Text
genSafeText = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

-- | Generates text that can be empty
genPotentiallyEmptySafeText :: Gen T.Text
genPotentiallyEmptySafeText = T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))


-- | Generates URL-safe characters (simplified)
genUrlChar :: Gen Char
genUrlChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~:/?#[]@!$&'()*+,;=")

-- | Generates a URL
genUrl :: Gen T.Text
genUrl = T.pack <$> listOf1 genUrlChar

-- | Generates content for inline code (no backticks or newlines)
genInlineCodeContent :: Gen T.Text
genInlineCodeContent = T.pack <$> listOf1 (arbitrary `suchThat` (\c -> c /= '`' && c /= '\n'))

-- | Generator for PlainText
genPlainText :: Gen InlineElement
genPlainText = PlainText <$> genSafeText

-- | Generator for InlineElement with recursion depth control
genSizedInlineElement :: Int -> Gen InlineElement
genSizedInlineElement 0 = genPlainText -- Base case: only PlainText to prevent infinite recursion
genSizedInlineElement n | n > 0 =
    frequency
        [ (5, genPlainText)
        , (1, CodeText <$> genInlineCodeContent)
        , (2, LinkText <$> genSizedInlineList (n-1) <*> genUrl)
        , (2, ImageText <$> genSafeText <*> genUrl)
        , (1, ItalicText <$> genSizedInlineList (n-1))
        , (1, BoldText <$> genSizedInlineList (n-1))
        ]
genSizedInlineElement _ = genPlainText -- Fallback

-- | Generator for a list of InlineElements with depth control
genSizedInlineList :: Int -> Gen [InlineElement]
genSizedInlineList n = sized $ \size -> do
    k <- choose (1, max 1 (min size 3)) -- Generate 1 to 3 elements to keep lists from being too long
    vectorOf k (genSizedInlineElement n)

-- | Main generator for InlineElement
genInlineElement :: Gen InlineElement
genInlineElement = sized genSizedInlineElement

-- | Generator for a list of InlineElements
genInlineElementList :: Gen [InlineElement]
genInlineElementList = sized genSizedInlineList

-- | Generator for Markdown header (text and expected element)
genHeaderMarkdown :: Gen (T.Text, MarkdownElement)
genHeaderMarkdown = do
    level <- choose (1, 6)
    contentList <- genInlineElementList `suchThat` (not . null) -- Headers must have content
    let hashes = T.replicate level "#"
    let textContent = T.concat $ map renderInlineForTest contentList -- Simple render for testing
    let mdInput = hashes <> " " <> textContent <> "\n"
    return (mdInput, Header level contentList)

-- | Generator for Markdown paragraph (text and expected element)
genParagraphMarkdown :: Gen (T.Text, MarkdownElement)
genParagraphMarkdown = do
    contentList <- genInlineElementList `suchThat` (not . null)
    let textContent = T.concat $ map renderInlineForTest contentList
    let mdInput = textContent <> "\n"
    return (mdInput, Paragraph contentList)

-- | Generator for Markdown code block (text and expected element)
genCodeBlockMarkdown :: Gen (T.Text, MarkdownElement)
genCodeBlockMarkdown = do
    lang <- frequency [(1, Just <$> genSafeText `suchThat` (\t -> not (T.null t) && not (T.any (== ' ') t) && not (T.any (== '\n') t))), (3, return Nothing)]
    codeLines <- listOf1 (genSafeText `suchThat` (not . T.isInfixOf "```")) -- Ensure no ``` inside code
    let codeContent = T.unlines codeLines
    let langStr = maybe "" (\l -> l <> "\n") lang
    let mdInput = "```" <> langStr <> codeContent <> "```\n"
    return (mdInput, CodeBlock lang (T.stripEnd codeContent))


-- | Generator for a bullet list item
genBulletListItem :: Gen (T.Text, [InlineElement])
genBulletListItem = do
    contentList <- genInlineElementList `suchThat` (not . null)
    marker <- elements ["*", "-"]
    let textContent = T.concat $ map renderInlineForTest contentList
    return (marker <> " " <> textContent, contentList)

-- | Generator for a Markdown bullet list
genBulletListMarkdown :: Gen (T.Text, MarkdownElement)
genBulletListMarkdown = do
    items <- listOf1 genBulletListItem
    let mdInput = T.unlines (map fst items) <> "\n"
    return (mdInput, BulletList (map snd items))

-- | Generator for a numbered list item
genNumberedListItem :: Gen (Int, T.Text, [InlineElement])
genNumberedListItem = do
    num <- choose (1, 99)
    contentList <- genInlineElementList `suchThat` (not . null)
    let textContent = T.concat $ map renderInlineForTest contentList
    return (num, T.pack (show num) <> ". " <> textContent, contentList)

-- | Generator for a Markdown numbered list
genNumberedListMarkdown :: Gen (T.Text, MarkdownElement)
genNumberedListMarkdown = do
    rawItems <- listOf1 genNumberedListItem
    -- For test simplicity, we won't handle renumbering here;
    -- the parser should correctly parse numbers as they are.
    let items = map (\(_, md, el) -> (md, el)) rawItems
    let mdInput = T.unlines (map fst items) <> "\n"
    return (mdInput, NumberedList (map snd items))

-- | Generator for a Markdown horizontal rule
genHorizontalRuleMarkdown :: Gen (T.Text, MarkdownElement)
genHorizontalRuleMarkdown = do
    marker <- elements ["---", "***", "___"] -- Though your parser currently only supports "---"
    padding <- T.pack <$> listOf (elements [' '])
    let mdInput = marker <> padding <> "\n"
    return (mdInput, HorizontalRule)


-- | Generator for MarkdownElement (excluding BlockQuote for initial test simplicity)
genMarkdownElement :: Gen MarkdownElement
genMarkdownElement = sized $ \s -> if s == 0 then fmap snd genParagraphMarkdown else -- Base case
    frequency
        [ (3, snd <$> genHeaderMarkdown)
        , (5, snd <$> genParagraphMarkdown)
        , (2, snd <$> genCodeBlockMarkdown)
        , (2, snd <$> genBulletListMarkdown)
        , (2, snd <$> genNumberedListMarkdown)
        , (1, return HorizontalRule)
        , (1, return EmptyLine)
        -- TODO: Add BlockQuote generator here
        -- , (1, BlockQuote <$> ...)
        ]

-- | Generator for MarkdownDoc
genMarkdownDoc :: Gen MarkdownDoc
genMarkdownDoc = MarkdownDoc <$> listOf genMarkdownElement


-- Helper function to "render" InlineElement to text for generators
-- This is a simplified version, only for generating input strings.
renderInlineForTest :: InlineElement -> T.Text
renderInlineForTest (PlainText t) = t
renderInlineForTest (ItalicText es) = "*" <> T.concat (map renderInlineForTest es) <> "*"
renderInlineForTest (BoldText es) = "**" <> T.concat (map renderInlineForTest es) <> "**"
renderInlineForTest (CodeText t) = "`" <> t <> "`"
renderInlineForTest (LinkText es url) = "[" <> T.concat (map renderInlineForTest es) <> "](" <> url <> ")"
renderInlineForTest (ImageText alt url) = "![" <> alt <> "](" <> url <> ")"

-- Generators for specific inline elements for more precise tests
genSpecificBold :: Gen (T.Text, InlineElement)
genSpecificBold = do
    contentList <- genSizedInlineList 1 `suchThat` (not . null) -- Limit nesting for simplicity
    marker <- elements ["**", "__"]
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, BoldText contentList)

genSpecificItalic :: Gen (T.Text, InlineElement)
genSpecificItalic = do
    contentList <- genSizedInlineList 1 `suchThat` (not . null) -- Limit nesting
    marker <- elements ["*", "_"]
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, ItalicText contentList)

genSpecificCodeText :: Gen (T.Text, InlineElement)
genSpecificCodeText = do
    content <- genInlineCodeContent
    return ("`" <> content <> "`", CodeText content)

genSpecificLinkText :: Gen (T.Text, InlineElement)
genSpecificLinkText = do
    altContentList <- genSizedInlineList 1 `suchThat` (not . null)
    url <- genUrl
    let renderedAlt = T.concat (map renderInlineForTest altContentList)
    return ("[" <> renderedAlt <> "](" <> url <> ")", LinkText altContentList url)

genSpecificImageText :: Gen (T.Text, InlineElement)
genSpecificImageText = do
    alt <- genSafeText `suchThat` (not . T.null) -- Alt text for images is usually simple
    url <- genUrl
    return ("![" <> alt <> "](" <> url <> ")", ImageText alt url)

