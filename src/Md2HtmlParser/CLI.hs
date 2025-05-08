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
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

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
run :: Options -> IO ()
run opts = do
  -- TODO: Implement the actual functionality
  pure ()
