{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T (Text, pack)
import Text.Megaparsec
import qualified Data.Map as M
import Control.Monad.State
import System.IO
import System.Environment

import Expression
import Statement

runTest :: String -> Int -> IO ()
runTest input expected =
  case runParser parseExpr "" (T.pack input) of
    Left err -> putStrLn $ "Parse error: " ++ errorBundlePretty err
    Right expr -> do
      print expr
      let computed = evalExpr expr M.empty
      let passed = computed == expected
      putStrLn $
        prettyPrintExpr expr ++ " == " ++ show computed ++ " <- " ++ show passed ++ "\n"

main :: IO ()
main = do
  runTest "1 + 2 * 3" 7
  runTest "-1 + 2 * 3" 5
  runTest "-(1 + 2) * 3" (-9)
  runTest "10 / 2 + 1" 6
  runTest "2 * (3 + 4)" 14
  runTest "1 + 2 + 3 + 4" 10
  runTest "18 / 3 / 3" 2
  runTest "-3 * -2" 6
  runTest "-3 * (2 + 1)" (-9)
  runTest "-1 + 2 *3 * 4 +-2" 21
  runTest "((-1) + (2 * 3) * 4 + (-2))" 21
  runTest "(-1 + 2) * 3 * (4 + (2))" 18
  runTest "(-(1 + 2) * 3) * (4 + (-2))" (-18)



