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

-- | Helper function to recursively merge consecutive ItalicText, BoldText, PlainText, or CodeText elements
-- within a list of InlineElements and also within nested elements.
mergeConsecutiveFormatting :: [InlineElement] -> [InlineElement]
mergeConsecutiveFormatting [] = []
mergeConsecutiveFormatting (x : xs) =
    -- Recursively merge content within nested elements first
    let processedX = case x of
            ItalicText es -> ItalicText (mergeConsecutiveFormatting es)
            BoldText es -> BoldText (mergeConsecutiveFormatting es)
            LinkText es url -> LinkText (mergeConsecutiveFormatting es) url
            -- ImageText and CodeText contain T.Text, not [InlineElement], so no recursion needed.
            _ -> x
        -- Process the rest of the list
        processedXs = mergeConsecutiveFormatting xs
    in
    -- Now merge the processed head (processedX) with the processed tail (processedXs)
    case (processedX, processedXs) of
        (PlainText t1, PlainText t2 : rest) ->
            -- Merge consecutive PlainText
            PlainText (t1 <> t2) : rest
        (ItalicText es1, ItalicText es2 : rest) ->
            -- Merge consecutive ItalicText
            ItalicText (es1 ++ es2) : rest
        (BoldText es1, BoldText es2 : rest) ->
            -- Merge consecutive BoldText
            BoldText (es1 ++ es2) : rest
        (CodeText c1, CodeText c2 : rest) ->
            -- Merge consecutive CodeText
            CodeText (c1 <> c2) : rest
        _ ->
            -- Otherwise, keep processedX and continue with processedXs
            processedX : processedXs


-- | Generator for a list of InlineElements with depth control and parent context, merging adjacent PlainText
-- Added a size parameter to limit the number of elements in the list.
genSizedInlineListWithParent :: Int -> Int -> Maybe InlineElementType -> Gen [InlineElement]
genSizedInlineListWithParent n listSize parentType = do
    -- Generate between 1 and listSize elements, or fewer if size is small
    k <- choose (1, max 1 (min listSize (n + 1))) -- Limit list size based on depth and a max size
    -- Generate the raw list using genSizedInlineElementWithParent
    rawList <- vectorOf k (genSizedInlineElementWithParent n parentType)
    -- Apply the recursive merging function to the generated list
    return $ mergeConsecutiveFormatting rawList


-- | Generator for InlineElement with recursion depth control and parent context
genSizedInlineElementWithParent :: Int -> Maybe InlineElementType -> Gen InlineElement
genSizedInlineElementWithParent 0 _ = genPlainText -- Base case: only PlainText to prevent infinite recursion
genSizedInlineElementWithParent n parentType | n > 0 =
    frequency $
        -- PlainText is always allowed, increase probability at lower depths
        (1, genPlainText) : -- Weight increases as n decreases (max depth 5 considered for scaling)
        -- CodeText: Not allowed inside CodeText or ImageText. Weight independent of depth as it's not recursive.
        (if parentType /= Just CodeTextType && parentType /= Just ImageTextType then [(2, CodeText <$> genInlineCodeContent)] else []) ++
        -- Recursive elements: weight proportional to current depth n. Apply mergeConsecutiveFormatting to the generated content list.
        -- Note: The recursive call to mergeConsecutiveFormatting is now handled within genSizedInlineListWithParent
        -- Limited nested list size to 3 for recursive elements
        (if parentType /= Just LinkTextType && parentType /= Just ImageTextType then [(4 * n, LinkText <$> genSizedInlineListWithParent (n-1) 3 (Just LinkTextType) <*> genUrl)] else []) ++
        (if parentType /= Just LinkTextType && parentType /= Just ImageTextType then [(4 * n, ImageText <$> genSafeText <*> genUrl)] else []) ++ -- Image alt text is T.Text, no recursive list here
        (if parentType /= Just ItalicTextType then [(2 * n, ItalicText <$> genSizedInlineListWithParent (n-1) 3 (Just ItalicTextType))] else []) ++
        (if parentType /= Just BoldTextType then [(2 * n, BoldText <$> genSizedInlineListWithParent (n-1) 3 (Just BoldTextType))] else [])

genSizedInlineElementWithParent _ _ = genPlainText -- Fallback for negative n

-- | Main generator for InlineElement (starts with no parent context)
genInlineElement :: Gen InlineElement
genInlineElement = sized $ \n -> genSizedInlineElementWithParent n Nothing

-- | Generator for a list of InlineElements (starts with no parent context)
-- Limited the top-level list size to 5
genInlineElementList :: Gen [InlineElement]
genInlineElementList = sized $ \n -> genSizedInlineListWithParent n 5 Nothing

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
    -- Limited the number of code lines to 5
    codeLines <- listOf1 (genSafeText `suchThat` (not . T.isInfixOf "```")) `suchThat` (\l -> length l <= 5)
    let codeContent = T.unlines codeLines
    let langPart = maybe "" id lang
    let mdInput = "```" <> langPart <> "\n" <> codeContent <> "```\n"
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
    -- Limited the number of list items to 5
    items <- listOf1 genBulletListItem `suchThat` (\l -> length l <= 5)
    let mdInput = T.unlines (map fst items)
    return (mdInput, BulletList (map snd items))

-- | Generator for a numbered list item
genNumberedListItem :: Gen (Int, T.Text, [InlineElement])
genNumberedListItem = do
    num <- choose (1, 10)
    -- List items contain inline elements with no parent context initially
    contentList <- genInlineElementList `suchThat` (not . null)
    let textContent = T.concat $ map renderInlineForTest contentList
    return (num, T.pack (show num) <> ". " <> textContent, contentList)

-- | Generator for a Markdown numbered list
genNumberedListMarkdown :: Gen (T.Text, MarkdownElement)
genNumberedListMarkdown = do
    -- Limited the number of list items to 5
    rawItems <- listOf1 genNumberedListItem `suchThat` (\l -> length l <= 5)
    -- For test simplicity, we won't handle renumbering here;
    -- the parser should correctly parse numbers as they are.
    let items = map (\(_, md, el) -> (md, el)) rawItems
    let mdInput = T.unlines (map fst items)
    return (mdInput, NumberedList (map snd items))

-- | Generator for a Markdown horizontal rule
genHorizontalRuleMarkdown :: Gen (T.Text, MarkdownElement)
genHorizontalRuleMarkdown = do
    marker <- elements ["---", "***", "___"] -- Though your parser currently only supports "---"
    padding <- T.pack <$> listOf (return ' ')
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
-- Limited the number of Markdown elements in the document to 10
genMarkdownDoc :: Gen MarkdownDoc
genMarkdownDoc = MarkdownDoc <$> listOf genMarkdownElement `suchThat` (\l -> length l <= 10)

-- Helper function to "render" InlineElement to text for generators
-- This is a simplified version, only for generating input strings.
renderInlineForTest :: InlineElement -> T.Text
renderInlineForTest (PlainText t) = t
renderInlineForTest (ItalicText es) = "_" <> T.concat (map renderInlineForTest es) <> "_"
renderInlineForTest (BoldText es) = "**" <> T.concat (map renderInlineForTest es) <> "**"
renderInlineForTest (CodeText t) = "`" <> t <> "`"
renderInlineForTest (LinkText es url) = "[" <> T.concat (map renderInlineForTest es) <> "](" <> url <> ")"
renderInlineForTest (ImageText alt url) = "![" <> alt <> "](" <> url <> ")"

-- Generators for specific inline elements for more precise tests
genSpecificBold :: Gen (T.Text, InlineElement)
genSpecificBold = do
    -- Bold content cannot contain BoldText, and consecutive elements are merged
    -- Limited nested list size to 3
    contentList <- genSizedInlineListWithParent 1 3 (Just BoldTextType) `suchThat` (not . null)
    let marker = "**"
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, BoldText contentList)

genSpecificItalic :: Gen (T.Text, InlineElement)
genSpecificItalic = do
    -- Italic content cannot contain ItalicText, and consecutive elements are merged
    -- Limited nested list size to 3
    contentList <- genSizedInlineListWithParent 1 3 (Just ItalicTextType) `suchThat` (not . null)
    let marker = "_"
    let renderedContent = T.concat (map renderInlineForTest contentList)
    return (marker <> renderedContent <> marker, ItalicText contentList)

genSpecificCodeText :: Gen (T.Text, InlineElement)
genSpecificCodeText = do
    -- CodeText content is just text, no recursion needed here.
    content <- genInlineCodeContent
    return ("`" <> content <> "`", CodeText content)

genSpecificLinkText :: Gen (T.Text, InlineElement)
genSpecificLinkText = do
    -- Link content cannot contain LinkText or ImageText, and consecutive elements are merged
    -- Limited nested list size to 3
    altContentList <- genSizedInlineListWithParent 1 3 (Just LinkTextType) `suchThat` (not . null)
    url <- genUrl
    let renderedAlt = T.concat (map renderInlineForTest altContentList)
    return ("[" <> renderedAlt <> "](" <> url <> ")", LinkText altContentList url)

genSpecificImageText :: Gen (T.Text, InlineElement)
genSpecificImageText = do
    -- Image alt text is just text, no recursion needed here.
    alt <- genSafeText `suchThat` (not . T.null)
    url <- genUrl
    return ("![" <> alt <> "](" <> url <> ")", ImageText alt url)
