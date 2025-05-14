module Main (main) where

import Md2HtmlParser.CLI (parseOptions, run)
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
  -- Set UTF-8 encoding for terminal output
  hSetEncoding stdout utf8
  
  -- Parse command line options and run the program
  options <- parseOptions
  run options

