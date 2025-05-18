{-# LANGUAGE OverloadedStrings #-}

module Main where

import Md2HtmlParser.Parser (parseMarkdownDoc, MarkdownDoc(..))
import Md2HtmlParser.Metrics (emptyMetrics, charCalls)
import Md2HtmlParser.GeneratorUtil (genMarkdownDocWithSizeAndDepth, renderMarkdownElementForTest)
import Control.Monad.State.Strict (runState)
import Text.Megaparsec (runParserT)
import qualified Data.Text as T
import Test.QuickCheck (generate)

runOnce :: Int -> Int -> IO (Int, Int, Int)
runOnce size depth = do
  doc <- generate $ genMarkdownDocWithSizeAndDepth size depth
  let markdown = T.concat (map renderMarkdownElementForTest (case doc of MarkdownDoc els -> els))
  let len = T.length markdown
  let parserAction = runParserT parseMarkdownDoc "" markdown
      (result, metrics) = runState parserAction emptyMetrics
  case result of
    Left _ -> return (len, 0, 0)
    Right _ -> return (len, charCalls metrics, 1)

runBatch :: Int -> Int -> Int -> IO ()
runBatch count size depth = do
  results <- mapM (const $ runOnce size depth) [1..count]
  let (totalLen, totalCalls, okCount) = foldr (\(l,c,s) (tl,tc,ts) -> (tl+l, tc+c, ts+s)) (0,0,0) results
  let avgLen = fromIntegral totalLen / fromIntegral count
  let avgCalls = fromIntegral totalCalls / fromIntegral count
  let ratio = if avgLen == 0 then 0 else avgCalls / avgLen
  putStrLn $ show size ++ "," ++ show depth ++ "," ++ show avgLen ++ "," ++ show avgCalls ++ "," ++ show ratio

main :: IO ()
main = do
  putStrLn "size,depth,avgLen,avgCalls,ratio"
  mapM_ (\(s,d) -> runBatch 100 s d)
    [ (s, d) | s <- [3,5,7,10,15,20], d <- [1,2,3,4,5] ]
