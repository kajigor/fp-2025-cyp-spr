module Main (main) where

import Md2HtmlParser.CLI (parseOptions, run)
import Md2HtmlParser.Logger (enableParserDebugging, printParserLogs)
import Md2HtmlParser.Parser (InlineElement (..), MarkdownDoc (..), MarkdownElement (..), 
                             parseMarkdownDoc, parseNumberedList, parseInlineElement)
import Md2HtmlParser (processMarkdown)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec (parse)
import System.IO (hSetEncoding, stdout, utf8)
import Prelude (print, IO, putStrLn)
import Prelude (Bool(True), Bool(False))

main :: IO ()
main = do
  -- Set UTF-8 encoding for terminal output
  hSetEncoding stdout utf8
  
  enableParserDebugging True
  
  putStrLn "Markdown to HTML Converter Examples:"

  putStrLn "\nExample 1: Headers and Text"
  let example1 = T.unlines
        [ T.pack "# Markdown Header"
        , T.pack ""
        , T.pack "This is a paragraph with **bold** and *italic* text."
        , T.pack ""
        , T.pack "## Subheading"
        , T.pack ""
        , T.pack "Another paragraph with `inline code`."
        ]
  putStrLn "Input Markdown:"
  TIO.putStrLn example1
  putStrLn "Output HTML:"
  TIO.putStrLn (processMarkdown example1)
  
  -- Example 2: Lists
  putStrLn "\nExample 2: Lists"
  let example2 = T.unlines
        [ T.pack "### Bullet List"
        , T.pack ""
        , T.pack "* First item"
        , T.pack "* Second item with **bold**"
        , T.pack "* Third item with `code`"
        , T.pack ""
        , T.pack "### Numbered List"
        , T.pack ""
        , T.pack "1. First numbered item"
        , T.pack "2. Second numbered item"
        , T.pack "3. Third numbered item"
        ]
  putStrLn "Input Markdown:"
  TIO.putStrLn example2
  putStrLn "Output HTML:"
  TIO.putStrLn (processMarkdown example2)
  
  -- Example 3: Links and Images
  putStrLn "\nExample 3: Links and Images"
  let example3 = T.unlines
        [ T.pack "Check out [this link](https://example.com)."
        , T.pack ""
        , T.pack "Here's an image: ![Alt text](image.jpg)"
        , T.pack ""
        , T.pack "A link with *formatting*: [**Bold Link**](https://bold.example.com)"
        ]
  putStrLn "Input Markdown:"
  TIO.putStrLn example3
  putStrLn "Output HTML:"
  TIO.putStrLn (processMarkdown example3)
  
  -- Example 4: Code Blocks
  putStrLn "\nExample 4: Code Blocks"
  let example4 = T.unlines
        [ T.pack "Regular code block:"
        , T.pack ""
        , T.pack "```"
        , T.pack "function example() {"
        , T.pack "  return 'Hello World';"
        , T.pack "}"
        , T.pack "```"
        , T.pack ""
        , T.pack "Haskell code block:"
        , T.pack ""
        , T.pack "```haskell"
        , T.pack "main :: IO ()"
        , T.pack "main = putStrLn \"Hello, World!\""
        , T.pack "```"
        ]
  putStrLn "Input Markdown:"
  TIO.putStrLn example4
  putStrLn "Output HTML:"
  TIO.putStrLn (processMarkdown example4)
  
  -- Example 5: Complex Document
  putStrLn "\nExample 5: Complex Document"
  let example5 = T.unlines
        [ T.pack "# Markdown Demo Document"
        , T.pack ""
        , T.pack "This is a paragraph with various formatting: **bold**, *italic*, and `code`."
        , T.pack ""
        , T.pack "## Lists"
        , T.pack ""
        , T.pack "### Bullet List"
        , T.pack "* Item with [a link](https://example.com)"
        , T.pack "* Item with *emphasis*"
        , T.pack "* Item with `code`"
        , T.pack ""
        , T.pack "### Numbered List"
        , T.pack "1. First item"
        , T.pack "2. Second item"
        , T.pack ""
        , T.pack "## Code Example"
        , T.pack ""
        , T.pack "```javascript"
        , T.pack "function greet(name) {"
        , T.pack "  return `Hello, ${name}!`;"
        , T.pack "}"
        , T.pack "```"
        , T.pack ""
        , T.pack "---"
        , T.pack ""
        , T.pack "Image example: ![Sample Image](sample.jpg)"
        ]
  putStrLn "Input Markdown:"
  TIO.putStrLn example5
  putStrLn "Output HTML:"
  TIO.putStrLn (processMarkdown example5)
  
  -- Print parser logs summary
  --  printParserLogs
  
  putStrLn "Done with examples."

