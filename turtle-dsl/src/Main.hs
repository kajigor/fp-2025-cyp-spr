module Main (main) where

import System.Environment (getArgs)

import qualified Parser as P
import qualified Interpreter as I

main :: IO ()
main = do
  args <- getArgs
  src  <- case args of
            [] -> getContents
            (f:_) -> readFile f
  case P.parseProgram src of
    Left err   -> putStrLn err
    Right prog -> case I.runProgram (fst prog) (snd prog) of
      Left err -> putStrLn err
      Right res -> putStrLn res

