{-# LANGUAGE OverloadedStrings #-}
module GeneratorUtil where

import Test.QuickCheck
import qualified Data.Text as T
import Md2HtmlParser.Parser (InlineElement(..), MarkdownElement(..), MarkdownDoc(..))
import Control.Monad (replicateM)
import Data.Maybe (isNothing)

-- | Data type to represent the type of InlineElement for context tracking
data InlineElementType = PlainTextType | ItalicTextType | BoldTextType | CodeTextType | LinkTextType | ImageTextType deriving (Eq, Show)

-- | Get the type of an InlineElement
getInlineElementType :: InlineElement -> InlineElementType
getInlineElementType PlainText{} = PlainTextType
getInlineElementType ItalicText{} = ItalicTextType
getInlineElementType BoldText{} = BoldTextType
getInlineElementType CodeText{} = CodeTextType
getInlineElementType LinkText{} = LinkTextType
getInlineElementType ImageText{} = ImageTextType

-- | Generates safe text (without special Markdown characters)
genSafeText :: Gen T.Text
genSafeText = T.pack <$> listOf1 (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

-- | Generates text that can be empty
genPotentiallyEmptySafeText :: Gen T.Text
genPotentiallyEmptySafeText = T.pack <$> listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

-- | Generates URL-safe characters (simplified)
genUrlChar :: Gen Char
genUrlChar = elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_.~:/?#@!$&*+,;=")

-- | Generates a URL
genUrl :: Gen T.Text
genUrl = T.pack <$> listOf1 genUrlChar

-- | Generates content for inline code (no backticks or newlines)
genInlineCodeContent :: Gen T.Text
genInlineCodeContent = T.pack <$> listOf1 (arbitrary `suchThat` (\c -> c /= '`' && c /= '\n'))

-- | Generator for PlainText
genPlainText :: Gen InlineElement
genPlainText = PlainText <$> genSafeText

-- | Helper function to merge adjacent PlainText elements in a list
mergeAdjacentPlainText :: [InlineElement] -> [InlineElement]
mergeAdjacentPlainText [] = []
mergeAdjacentPlainText [x] = [x]
mergeAdjacentPlainText (PlainText t1 : PlainText t2 : xs) =
    mergeAdjacentPlainText (PlainText (t1 <> t2) : xs)
mergeAdjacentPlainText (x : xs) = x : mergeAdjacentPlainText xs

-- | Generator for a list of InlineElements with depth control and parent context, merging adjacent PlainText
genSizedInlineListWithParent :: Int -> Maybe InlineElementType -> Gen [InlineElement]
genSizedInlineListWithParent n parentType = sized $ \size -> do
    k <- choose (1, max 1 (min size 3)) -- Generate 1 to 3 elements initially
    -- Generate the raw list using genSizedInlineElementWithParent
    rawList <- vectorOf k (genSizedInlineElementWithParent n parentType)
    -- Merge adjacent PlainText elements
    return $ mergeAdjacentPlainText rawList

-- | Generator for InlineElement with recursion depth control and parent context
genSizedInlineElementWithParent :: Int -> Maybe InlineElementType -> Gen InlineElement
genSizedInlineElementWithParent 0 _ = genPlainText -- Base case: only PlainText to prevent infinite recursion
genSizedInlineElementWithParent n parentType | n > 0 =
    frequency $
        -- PlainText is always allowed
        (10, genPlainText) :
        -- CodeText: Not allowed inside CodeText or ImageText
        (if parentType /= Just CodeTextType && parentType /= Just ImageTextType then [(2, CodeText <$> genInlineCodeContent)] else []) ++
        -- LinkText: Not allowed inside LinkText or ImageText
        (if parentType /= Just LinkTextType && parentType /= Just ImageTextType then [(4, LinkText <$> genSizedInlineListWithParent (n-1) (Just LinkTextType) <*> genUrl)] else []) ++
        -- ImageText: Not allowed inside LinkText or ImageText. Alt text is T.Text, no recursion needed for content list.
        (if parentType /= Just LinkTextType && parentType /= Just ImageTextType then [(4, ImageText <$> genSafeText <*> genUrl)] else []) ++
        -- ItalicText: Not allowed inside ItalicText
        (if parentType /= Just ItalicTextType then [(2, ItalicText <$> genSizedInlineListWithParent (n-1) (Just ItalicTextType))] else []) ++
        -- BoldText: Not allowed inside BoldText
        (if parentType /= Just BoldTextType then [(2, BoldText <$> genSizedInlineListWithParent (n-1) (Just BoldTextType))] else [])

genSizedInlineElementWithParent _ _ = genPlainText -- Fallback for negative n

-- | Main generator for InlineElement (starts with no parent context)
genInlineElement :: Gen InlineElement
genInlineElement = sized $ \n -> genSizedInlineElementWithParent n Nothing

-- | Generator for a list of InlineElements (starts with no parent context)
genInlineElementList :: Gen [InlineElement]
genInlineElementList = sized $ \n -> genSizedInlineListWithParent n Nothing

-- | Generator for Markdown header (text and expected element)
genHeaderMarkdown :: Gen (T.Text, MarkdownElement)
genHeaderMarkdown = do
    level <- choose (1, 6)
    -- Headers contain inline elements with no parent context initially
    contentList <- genInlineElementList `suchThat` (not . null)
    let hashes = T.replicate level "#"
    let textContent = T.concat $ map renderInlineForTest contentList -- Simple render for testing
    let mdInput = hashes <> " " <> textContent <> "\n"
    return (mdInput, Header level contentList)

-- | Generator for Markdown paragraph (text and expected element)
genParagraphMarkdown :: Gen (T.Text, MarkdownElement)
genParagraphMarkdown = do
    -- Paragraphs contain inline elements with no parent context initially
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
    -- List items contain inline elements with no parent context initially
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
    -- List items contain inline elements with no parent context initially
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
    -- Bold content cannot contain BoldText
    contentList <- genSizedInlineListWithParent 1 (Just BoldTextType) `suchThat` (not . null)
    marker <- elements ["**", "__"]
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, BoldText contentList)

genSpecificItalic :: Gen (T.Text, InlineElement)
genSpecificItalic = do
    -- Italic content cannot contain ItalicText
    contentList <- genSizedInlineListWithParent 1 (Just ItalicTextType) `suchThat` (not . null)
    marker <- elements ["*", "_"]
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, ItalicText contentList)

genSpecificCodeText :: Gen (T.Text, InlineElement)
genSpecificCodeText = do
    -- CodeText content is just text, no recursion needed here.
    content <- genInlineCodeContent
    return ("`" <> content <> "`", CodeText content)

genSpecificLinkText :: Gen (T.Text, InlineElement)
genSpecificLinkText = do
    -- Link content cannot contain LinkText or ImageText
    altContentList <- genSizedInlineListWithParent 1 (Just LinkTextType) `suchThat` (not . null)
    url <- genUrl
    let renderedAlt = T.concat (map renderInlineForTest altContentList)
    return ("[" <> renderedAlt <> "](" <> url <> ")", LinkText altContentList url)

genSpecificImageText :: Gen (T.Text, InlineElement)
genSpecificImageText = do
    -- Image alt text is just text, no recursion needed here.
    alt <- genSafeText `suchThat` (not . T.null)
    url <- genUrl
    return ("![" <> alt <> "](" <> url <> ")", ImageText alt url)
