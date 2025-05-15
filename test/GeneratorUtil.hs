{-# LANGUAGE OverloadedStrings #-}

module GeneratorUtil where

-- Assuming this is a local module or from a dependency
import Control.Monad (replicateM)
import Data.List (groupBy) -- Added for the optimized mergeConsecutiveFormatting
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Md2HtmlParser.Parser (InlineElement (..), MarkdownDoc (..), MarkdownElement (..))
import Test.QuickCheck

-- | Data type to represent the type of InlineElement for context tracking
data InlineElementType = PlainTextType | ItalicTextType | BoldTextType | CodeTextType | LinkTextType | ImageTextType deriving (Eq, Show)

-- | Get the type of an InlineElement
getInlineElementType :: InlineElement -> InlineElementType
getInlineElementType PlainText {} = PlainTextType
getInlineElementType ItalicText {} = ItalicTextType
getInlineElementType BoldText {} = BoldTextType
getInlineElementType CodeText {} = CodeTextType
getInlineElementType LinkText {} = LinkTextType
getInlineElementType ImageText {} = ImageTextType

-- | Generates safe text (without special Markdown characters)
genSafeText :: Gen T.Text
genSafeText = T.pack <$> listOf1 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " "))

-- | Generates text that can be empty
genPotentiallyEmptySafeText :: Gen T.Text
genPotentiallyEmptySafeText = T.pack <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " "))

-- | Generates URL-safe characters (simplified)
genUrlChar :: Gen Char
genUrlChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_.~:/?#@!$&*+,;=")

genCodeChar :: Gen Char
genCodeChar = genUrlChar

-- | Generates a URL
genUrl :: Gen T.Text
genUrl = T.pack <$> listOf1 genUrlChar

-- | Generates content for inline code (no backticks or newlines)
genInlineCodeContent :: Gen T.Text
genInlineCodeContent = T.pack <$> listOf1 (genCodeChar `suchThat` (\c -> c /= '`' && c /= '\n'))

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
        _ -> x
      processedXs = mergeConsecutiveFormatting xs
   in
      case (processedX, processedXs) of
        (PlainText t1, PlainText t2 : rest) ->
          PlainText (t1 <> t2) : rest
        (ItalicText es1, ItalicText es2 : rest) ->
          ItalicText (mergeConsecutiveFormatting (es1 ++ es2)) : rest
        (BoldText es1, BoldText es2 : rest) ->
          BoldText (mergeConsecutiveFormatting (es1 ++ es2)) : rest
        (CodeText c1, CodeText c2 : rest) ->
          CodeText (c1 <> c2) : rest
        _ ->
          processedX : processedXs

-- | Generator for a list of InlineElements with depth control and parent context, merging adjacent PlainText
-- Added a size parameter to limit the number of elements in the list.
genSizedInlineListWithParent :: Int -> Int -> Maybe InlineElementType -> Gen [InlineElement]
genSizedInlineListWithParent n listSize parentType = do
  elems <- genNotOptimizeSizedInlineListWithParent n listSize parentType
  return $ mergeConsecutiveFormatting elems

genNotOptimizeSizedInlineListWithParent :: Int -> Int -> Maybe InlineElementType -> Gen [InlineElement]
genNotOptimizeSizedInlineListWithParent n listSize parentType = do
  -- Generate between 1 and listSize elements, or fewer if size is small
  k <- choose (1, max 1 (min listSize (n + 1))) -- Limit list size based on depth and a max size
  -- Generate the raw list using genSizedInlineElementWithParent
  rawList <- vectorOf k (genSizedInlineElementWithParent n parentType)
  -- Apply the recursive merging function to the generated list
  return rawList

-- | Generator for InlineElement with recursion depth control and parent context
genSizedInlineElementWithParent :: Int -> Maybe InlineElementType -> Gen InlineElement
genSizedInlineElementWithParent 0 _ = genPlainText -- Base case: only PlainText to prevent infinite recursion
genSizedInlineElementWithParent n parentType
  | n > 0 =
      frequency $
        (10 + (5 - min 5 n) * 3, genPlainText)
          : ([(2, CodeText <$> genInlineCodeContent) | parentType /= Just CodeTextType && parentType /= Just ImageTextType])
          ++ ([(4 * n, LinkText <$> genNotOptimizeSizedInlineListWithParent (n - 1) 3 (Just LinkTextType) <*> genUrl) | parentType /= Just LinkTextType && parentType /= Just ImageTextType])
          ++ ([(4 * n, ImageText <$> genSafeText <*> genUrl) | parentType /= Just LinkTextType && parentType /= Just ImageTextType])
          ++ ([(2 * n, ItalicText <$> genNotOptimizeSizedInlineListWithParent (n - 1) 3 (Just ItalicTextType)) | parentType /= Just ItalicTextType])
          ++ ([(2 * n, BoldText <$> genNotOptimizeSizedInlineListWithParent (n - 1) 3 (Just BoldTextType)) | parentType /= Just BoldTextType])
genSizedInlineElementWithParent _ _ = genPlainText

-- | Main generator for InlineElement (starts with no parent context)
genInlineElement :: Gen InlineElement
genInlineElement = sized $ \n -> do
  rawElem <- genSizedInlineElementWithParent (min 3 n) Nothing
  let mergedList = mergeConsecutiveFormatting [rawElem]
  case mergedList of
    (firstElem : _) -> return firstElem
    _ -> error "Internal error: mergeConsecutiveFormatting returned an empty list for a single-element input."

genInlineElementList :: Gen [InlineElement]
genInlineElementList = sized $ \n -> do
  rawList <- genNotOptimizeSizedInlineListWithParent (min 5 n) 5 Nothing
  return $ mergeConsecutiveFormatting rawList

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
  num <- choose (1, 99)
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
genMarkdownElement = sized $ \s ->
  if s == 0
    then fmap snd genParagraphMarkdown -- Base case
    else
      frequency
        [ (3, snd <$> genHeaderMarkdown),
          (5, snd <$> genParagraphMarkdown),
          (2, snd <$> genCodeBlockMarkdown),
          (2, snd <$> genBulletListMarkdown),
          (2, snd <$> genNumberedListMarkdown),
          (1, return HorizontalRule),
          (1, return EmptyLine)
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
