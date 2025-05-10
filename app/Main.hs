module Main (main) where

import Md2HtmlParser.CLI (parseOptions, run)
import Md2HtmlParser.Logger (enableParserDebugging, printParserLogs)
import Md2HtmlParser.Parser (InlineElement (..), MarkdownDoc (..), MarkdownElement (..), 
                             parseMarkdownDoc, parseNumberedList, parseInlineElement)
import qualified Data.Text as T
import Text.Megaparsec (parse)
import System.IO (hSetEncoding, stdout, utf8)
import Prelude (print, IO, putStrLn, )
import Prelude ( Bool( True ) )

main :: IO ()
main = do
  -- Set UTF-8 encoding for terminal output
  hSetEncoding stdout utf8
  
  -- Enable parser debugging
  enableParserDebugging True
  
  putStrLn "Parsing Markdown examples:"

--  -- Try a few different examples to see the logging in action
--  putStrLn "\nParsing a simple list with code:"
--  print ( parse parseMarkdownDoc ""
--    (T.pack " * List with `code`\n")
--    )
--
--  putStrLn "\nParsing nested formatting:"
--  print ( parse parseInlineElement ""
--    (T.pack "*italic with **bold** inside*")
--    )
--
--  putStrLn "\nParsing a complex document:"
--  print ( parse parseMarkdownDoc ""
--    (T.pack "# Header\n\n  1. First item\n  2. Second item\n\n  * Bullet item\n\n---\n")
--    )
    
--  putStrLn "\nParsing a list with code followed by horizontal rule:"
--  print ( parse parseMarkdownDoc ""
--    (T.pack "  * List with `code`\n---")
--    )

--  print ( parse parseMarkdownDoc ""
--    (T.pack "text\n`code`\n---")
--    )
--  print ( parse parseInlineElement ""
--    (T.pack "`code`\n")
--    )
  
  -- Print parser logs summary
  printParserLogs
  
  putStrLn "Done parsing examples."

