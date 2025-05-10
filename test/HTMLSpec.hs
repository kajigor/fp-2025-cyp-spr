{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module HTMLSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import Md2HtmlParser.Parser
import Md2HtmlParser.HTML (HtmlDoc(..), HtmlElement(..), HtmlAttribute(..), renderElement, renderAttributes)
import Md2HtmlParser.HTML
import Md2HtmlParser (processMarkdown)
import Data.Text (Text)

spec :: Spec
spec = do
  describe "HTML Element Rendering" $ do
    it "renders plain text correctly" $ do
      let element = HtmlText (T.pack "Simple text")
      renderElement element `shouldBe` T.pack "Simple text"
      
    it "escapes HTML special characters in text" $ do
      let element = HtmlText (T.pack "<script> & \"quote\" & 'apostrophe'")
      renderElement element `shouldBe` T.pack "&lt;script&gt; &amp; &quot;quote&quot; &amp; &#39;apostrophe&#39;"
      
    it "renders void elements correctly" $ do
      let hr = HtmlTag (T.pack "hr") [] []
      renderElement hr `shouldBe` T.pack "<hr>"
      
      let br = HtmlTag (T.pack "br") [] []
      renderElement br `shouldBe` T.pack "<br>"
      
      let img = HtmlTag (T.pack "img") [HtmlAttribute (T.pack "src") (T.pack "image.jpg"), HtmlAttribute (T.pack "alt") (T.pack "An image")] []
      renderElement img `shouldBe` T.pack "<img src=\"image.jpg\" alt=\"An image\">"
      
    it "renders container elements with children correctly" $ do
      let div = HtmlTag (T.pack "div") [] [
                  HtmlTag (T.pack "p") [] [HtmlText (T.pack "Paragraph 1")],
                  HtmlTag (T.pack "p") [] [HtmlText (T.pack "Paragraph 2")]
                ]
      renderElement div `shouldBe` T.pack "<div><p>Paragraph 1</p><p>Paragraph 2</p></div>"
      
    it "renders nested elements correctly" $ do
      let nested = HtmlTag (T.pack "ul") [] [
                     HtmlTag (T.pack "li") [] [
                       HtmlTag (T.pack "a") [HtmlAttribute (T.pack "href") (T.pack "link")] [HtmlText (T.pack "Link text")]
                     ]
                   ]
      renderElement nested `shouldBe` T.pack "<ul><li><a href=\"link\">Link text</a></li></ul>"
      
    it "renders HTML attributes correctly" $ do
      let attrs = [HtmlAttribute (T.pack "class") (T.pack "highlight"), HtmlAttribute (T.pack "id") (T.pack "header")]
      renderAttributes attrs `shouldBe` T.pack " class=\"highlight\" id=\"header\""
      
  describe "Markdown to HTML Conversion" $ do
    it "converts headers correctly" $ do
      let md = MarkdownDoc [Header 1 [PlainText (T.pack "Header 1")]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<h1>Header 1</h1>") html `shouldBe` True
      
    it "converts nested formatting correctly" $ do
      let md = MarkdownDoc [Paragraph [PlainText (T.pack "Normal "), BoldText [PlainText (T.pack "bold "), ItalicText [PlainText (T.pack "and italic")]]]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<p>Normal <strong>bold <em>and italic</em></strong></p>") html `shouldBe` True
      
    it "converts paragraphs correctly" $ do
      let md = MarkdownDoc [Paragraph [PlainText (T.pack "This is a paragraph.")]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<p>This is a paragraph.</p>") html `shouldBe` True
      
    it "converts bullet lists correctly" $ do
      let md = MarkdownDoc [BulletList [[PlainText (T.pack "Item 1")], [PlainText (T.pack "Item 2")]]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<ul><li>Item 1</li><li>Item 2</li></ul>") html `shouldBe` True
      
    it "converts numbered lists correctly" $ do
      let md = MarkdownDoc [NumberedList [[PlainText (T.pack "First")], [PlainText (T.pack "Second")]]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<ol><li>First</li><li>Second</li></ol>") html `shouldBe` True
      
    it "converts code blocks correctly" $ do
      let md = MarkdownDoc [CodeBlock Nothing (T.pack "code block")]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<pre><code>code block</code></pre>") html `shouldBe` True
      
    it "converts code blocks with language correctly" $ do
      let md = MarkdownDoc [CodeBlock (Just (T.pack "haskell")) (T.pack "main = putStrLn \"Hello\"")]
      let html = renderHtml $ markdownToHtml md
      print html
      T.isInfixOf (T.pack "<pre><code class=\"language-haskell\">main = putStrLn  &quot;Hello&quot;</code></pre>") html `shouldBe` True
      
    it "converts links correctly" $ do
      let md = MarkdownDoc [Paragraph [LinkText [PlainText (T.pack "Google")] (T.pack "https://google.com")]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<a href=\"https://google.com\">Google</a>") html `shouldBe` True
      
    it "converts images correctly" $ do
      let md = MarkdownDoc [Paragraph [ImageText (T.pack "Alt text") (T.pack "image.jpg")]]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<img src=\"image.jpg\" alt=\"Alt text\">") html `shouldBe` True
      
    it "converts horizontal rules correctly" $ do
      let md = MarkdownDoc [HorizontalRule]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<hr>") html `shouldBe` True
      
    it "converts multiple elements correctly" $ do
      let md = MarkdownDoc [
                Header 1 [PlainText (T.pack "Title")],
                Paragraph [PlainText (T.pack "A paragraph with "), LinkText [PlainText (T.pack "a link")] (T.pack "url")],
                BulletList [[PlainText (T.pack "Item 1")], [PlainText (T.pack "Item 2")]]
              ]
      let html = renderHtml $ markdownToHtml md
      T.isInfixOf (T.pack "<h1>Title</h1>") html `shouldBe` True
      T.isInfixOf (T.pack "<p>A paragraph with <a href=\"url\">a link</a></p>") html `shouldBe` True
      T.isInfixOf (T.pack "<ul><li>Item 1</li><li>Item 2</li></ul>") html `shouldBe` True

  describe "End-to-End Markdown to HTML Conversion" $ do
    it "processes simple markdown" $ do
      let markdown = T.unlines [
            T.pack "# Header",
            T.pack "",
            T.pack "Paragraph with **bold** and *italic* text.",
            T.pack "",
            T.pack "- List item 1",
            T.pack "- List item 2",
            T.pack "",
            T.pack "```haskell",
            T.pack "main = putStrLn \"Hello\"",
            T.pack "```"
            ]
      print markdown
      let html = processMarkdown markdown
      putStrLn $ "Generated HTML: " ++ T.unpack html
      print html
      T.isInfixOf (T.pack "<h1>Header</h1>") html `shouldBe` True
      T.isInfixOf (T.pack "<p>Paragraph with <strong>bold</strong> and <em>italic</em> text.</p>") html `shouldBe` True
      T.isInfixOf (T.pack "<ul><li>List item 1</li><li>List item 2</li></ul>") html `shouldBe` True
      T.isInfixOf (T.pack "<pre><code class=\"language-haskell\">main = putStrLn &quot;Hello&quot;</code></pre>") html `shouldBe` True
  
    it "properly escapes HTML in content" $ do
      let markdown = T.pack "This has <script> tags & special \"characters\""
      let html = processMarkdown markdown
      T.isInfixOf (T.pack "&lt;script&gt;") html `shouldBe` True
      T.isInfixOf (T.pack "&amp;") html `shouldBe` True
      T.isInfixOf (T.pack "&quot;characters&quot;") html `shouldBe` True
