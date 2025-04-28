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

main :: IO ()
main = do
  putStrLn "Main application"



