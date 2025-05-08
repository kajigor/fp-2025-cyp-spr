module Main (main) where

import Md2HtmlParser.CLI (parseOptions, run)

main :: IO ()
main = parseOptions >>= run
