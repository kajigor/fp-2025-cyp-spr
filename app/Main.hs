module Main (main) where

import Md2HtmlParser.CLI (parseOptions, run)
import Md2HtmlParser.Parser (InlineElement (..), MarkdownDoc (..), MarkdownElement (..), parseMarkdownDoc, parseNumberedList)
import qualified Data.Text as T
import Text.Megaparsec (parse)
import System.IO (hSetEncoding, stdout, utf8)
import Prelude (print, IO)

main :: IO ()
main = do
  -- Set UTF-8 encoding for terminal output
  hSetEncoding stdout utf8

--  -- This should fail with an error since it's malformed
--  print (parse parseInlineElement "" (T.pack "*outer **inner*"))
--
--  -- This is the correct way to do nested formatting
--  print (parse parseInlineElement "" (T.pack "*outer **inner** rest*"))
-- (T.pack "# Header\n\n  1. First item\n  2. Second item\n\n  * Bullet item\n\n---\n")
  print ( parse parseMarkdownDoc ""
    (T.pack "  1. First item\n  2. Second item\n---")
    )
  print ( parse parseNumberedList ""
   (T.pack "  1. First item\n  2. Second item")
    )

