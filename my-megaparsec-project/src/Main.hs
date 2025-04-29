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
import System.Directory (doesFileExist)

import Statement (Stmt, evalStmt, parseProgram, prettyPrintStmt)
import Optimizer (optimizeWithEmptySubst)

when :: Bool -> IO () -> IO ()
when condition action = if condition then action else return ()

data Options = Options
  { programText :: Maybe String  -- The program source code or file path
  , programArgs :: [Int]         -- Input arguments
  , optimize    :: Bool          -- Whether to optimize the program
  , verbose     :: Bool          -- Whether to print verbose output
  , filePath    :: Maybe String  -- Optional path to file containing the program
  }

argsParser :: Parser [Int]
argsParser = option (str >>= readIntList)
  ( long "args"
  <> short 'a'
  <> metavar "ARGS"
  <> value []
  <> help "Integer arguments for the program (comma-separated list or [x,y,z] array format)"
  )
  where
    readIntList :: String -> ReadM [Int]
    readIntList "" = return []
    readIntList s
      | head s == '[' && last s == ']' =
          case reads s of
            [(xs, "")] -> return xs
            _          -> readerError "Invalid array format. Use [x,y,z] or comma-separated integers."
      | otherwise =
          case reads ("[" ++ s ++ "]") of
            [(xs, "")] -> return xs
            _          -> readerError "Invalid argument format. Use comma-separated integers or [x,y,z]."

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (argument str
      ( metavar "PROGRAM | FILE"
      <> help "Program source code or file path"
      ))
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
  <*> optional (strOption
      ( long "file"
      <> short 'f'
      <> metavar "FILE"
      <> help "Read program from file (alternative to providing file as PROGRAM)"
      ))

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

readProgramFromFile :: String -> IO String
readProgramFromFile path = do
  exists <- doesFileExist path
  if exists
    then readFile path
    else do
      hPutStrLn stderr $ "Error: File not found: " ++ path
      exitFailure

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Parse and execute programs in the statement language"
     <> header "parse - Statement parser and interpreter" )

run :: Options -> IO ()
run opts = do
  programSource <- case (programText opts, filePath opts) of
    (_, Just path) -> readProgramFromFile path
    
    (Just program, Nothing) -> do
      exists <- doesFileExist program
      if exists
        then readProgramFromFile program -- if --file spesified PROGRAM is a file name
        else return program
        
    -- Neither provided
    (Nothing, Nothing) -> do
      hPutStrLn stderr "Error: No program source or file path provided."
      exitFailure

  program <- parseInputProgram programSource
  
  when (verbose opts) $ do
    putStrLn "Program text:"
    putStrLn programSource
    putStrLn ""
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