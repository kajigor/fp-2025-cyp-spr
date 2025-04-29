{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Options.Applicative
import Control.Monad.State (evalState)
import qualified Data.Map as M

import qualified Data.Text as T
import Text.Megaparsec (runParser, errorBundlePretty)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import Control.Exception (try, SomeException)

import Statement (Stmt, evalStmt, parseProgram, prettyPrintStmt)
import Optimizer (optimizeWithEmptySubst)

when :: Bool -> IO () -> IO ()
when condition action = if condition then action else return ()

data Options = Options
  { programText :: String  -- The program source code
  , programArgs :: [Int]   -- Input arguments
  , optimize    :: Bool    -- Whether to optimize the program
  , verbose     :: Bool    -- Whether to print verbose output
  }

argsParser :: Parser [Int]
argsParser = option (str >>= readIntList)
  ( long "args"
  <> short 'a'
  <> metavar "ARGS"
  <> value []
  <> help "Comma-separated list of integer arguments for the program"
  )
  where
    readIntList :: String -> ReadM [Int]
    readIntList "" = return []
    readIntList s =
      case reads ("[" ++ s ++ "]") of
        [(xs, "")] -> return xs
        _          -> readerError "Invalid argument format. Use comma-separated integers."

optionsParser :: Parser Options
optionsParser = Options
  <$> argument str
      ( metavar "PROGRAM"
      <> help "Program to parse and run"
      )
  <*> argsParser
  <*> switch
      ( long "optimize"
      <> short 'o'
      <> help "Optimize the program before execution"
      )
  <*> switch
      ( long "verbose"
      <> short 'v'
      <> help "Print verbose output including the parsed AST"
      )

parseInputProgram :: String -> IO Stmt
parseInputProgram input = do
  case runParser parseProgram "" (T.pack input) of
    Left err -> do
      hPutStrLn stderr $ "Parse error: " ++ errorBundlePretty err
      exitFailure
    Right stmt -> return stmt

formatOutput :: [Int] -> String
formatOutput [] = "No output"
formatOutput [x] = "Output: " ++ show x
formatOutput xs = "Output: " ++ show xs

runProgram :: Stmt -> [Int] -> IO [Int]
runProgram stmt args = do
  return $ evalState (evalStmt stmt) (M.empty, args)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Parse and execute programs in the statement language"
     <> header "parse - Statement parser and interpreter" )

run :: Options -> IO ()
run opts = do
  program <- parseInputProgram (programText opts)
  
  when (verbose opts) $ do
    putStrLn "Parsed program:"
    putStrLn $ prettyPrintStmt program
    putStrLn ""
  
  let finalProgram = if optimize opts
                     then optimizeWithEmptySubst program
                     else program
  
  when (optimize opts && verbose opts) $ do
    putStrLn "Optimized program:"
    putStrLn $ prettyPrintStmt finalProgram
    putStrLn ""
  
  result <- try (runProgram finalProgram (programArgs opts)) :: IO (Either SomeException [Int])
  case result of
    Left err -> do
      hPutStrLn stderr $ "Runtime error: " ++ show err
      exitFailure
    Right output -> putStrLn $ formatOutput output