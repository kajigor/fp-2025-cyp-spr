{-# LANGUAGE OverloadedStrings #-}

module Md2HtmlParser.CLI
  ( parseOptions
  , run
  , Options(..)
  ) where

import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr, stdin, stdout)
import System.Exit (exitFailure)
import Control.Monad (when)

import Md2HtmlParser (processMarkdown)
import Md2HtmlParser.Logger (enableParserDebugging)

-- | Command line options
data Options = Options
  { optInput    :: Maybe FilePath  -- ^ Input file path (Nothing means stdin)
  , optOutput   :: Maybe FilePath  -- ^ Output file path (Nothing means stdout)
  , optVerbose  :: Bool            -- ^ Verbose output
  } deriving (Show, Eq)

-- | Parser for input file option
inputParser :: Parser (Maybe FilePath)
inputParser = optional $ strOption
  ( long "input"
  <> short 'i'
  <> metavar "FILENAME"
  <> help "Input markdown file (default: stdin)")

-- | Parser for output file option
outputParser :: Parser (Maybe FilePath)
outputParser = optional $ strOption
  ( long "output"
  <> short 'o'
  <> metavar "FILENAME"
  <> help "Output HTML file (default: stdout)")

-- | Parser for verbose flag
verboseParser :: Parser Bool
verboseParser = switch
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose output")

-- | Combined parser for all options
optionsParser :: Parser Options
optionsParser = Options
  <$> inputParser
  <*> outputParser
  <*> verboseParser

-- | Parse command-line arguments
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
      <> progDesc "Convert Markdown to HTML"
      <> header "Md2HtmlParser - a markdown to HTML converter")

-- | Log a message if verbose mode is enabled
logMessage :: Bool -> String -> IO ()
logMessage verbose msg = when verbose $ putStrLn msg

-- | Read input from a file or stdin
readInput :: Options -> IO T.Text
readInput opts = do
  logMessage (optVerbose opts) "Reading input..."
  case optInput opts of
    Nothing -> do
      logMessage (optVerbose opts) "Reading from stdin"
      TIO.getContents
    Just filePath -> do
      fileExists <- doesFileExist filePath
      if fileExists
        then do
          logMessage (optVerbose opts) $ "Reading from file: " ++ filePath
          TIO.readFile filePath
        else do
          hPutStrLn stderr $ "Error: Input file does not exist: " ++ filePath
          exitFailure

-- | Write output to a file or stdout
writeOutput :: Options -> T.Text -> IO ()
writeOutput opts html = do
  logMessage (optVerbose opts) "Writing output..."
  case optOutput opts of
    Nothing -> do
      logMessage (optVerbose opts) "Writing to stdout"
      TIO.putStrLn html
    Just filePath -> do
      logMessage (optVerbose opts) $ "Writing to file: " ++ filePath
      TIO.writeFile filePath html
      logMessage (optVerbose opts) $ "Output written to: " ++ filePath

-- | Run the markdown to HTML conversion process
run :: Options -> IO ()
run opts = do
  -- Enable debugging if verbose mode is on
  enableParserDebugging (optVerbose opts)
  
  -- Log program start
  logMessage (optVerbose opts) "Starting Markdown to HTML conversion"
  
  -- Read input
  markdownText <- readInput opts
  
  -- Process markdown to HTML
  logMessage (optVerbose opts) "Converting Markdown to HTML"
  let htmlOutput = processMarkdown markdownText
  
  -- Write output
  writeOutput opts htmlOutput
  
  -- Log completion
  logMessage (optVerbose opts) "Conversion completed successfully"
