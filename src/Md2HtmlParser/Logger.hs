module Md2HtmlParser.Logger
  ( logInfo,
    logDebug,
    withLogging,
    enableDebugging,
    isDebugEnabled,
    enableParserDebugging,
    isParserDebugEnabled,
    logParserCall,
    logParserResult,
    printParserLogs
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)

-- Global debug flags (using unsafePerformIO for simplicity in this context)
{-# NOINLINE debugEnabled #-}
debugEnabled :: IORef Bool
debugEnabled = unsafePerformIO $ newIORef False

{-# NOINLINE parserDebugEnabled #-}
parserDebugEnabled :: IORef Bool
parserDebugEnabled = unsafePerformIO $ newIORef False

-- | Enable or disable regular debugging
enableDebugging :: Bool -> IO ()
enableDebugging flag = writeIORef debugEnabled flag

-- | Check if debugging is enabled
isDebugEnabled :: IO Bool
isDebugEnabled = readIORef debugEnabled

-- | Enable or disable parser debugging
enableParserDebugging :: Bool -> IO ()
enableParserDebugging flag = writeIORef parserDebugEnabled flag

-- | Check if parser debugging is enabled
isParserDebugEnabled :: IO Bool
isParserDebugEnabled = readIORef parserDebugEnabled

-- | Log an informational message
logInfo :: MonadIO m => Text -> m ()
logInfo msg = liftIO $ hPutStrLn stderr $ "[INFO] " ++ T.unpack msg

-- | Log a debug message
logDebug :: MonadIO m => Text -> m ()
logDebug msg = liftIO $ do
  enabled <- isDebugEnabled
  if enabled
    then hPutStrLn stderr $ "[DEBUG] " ++ T.unpack msg
    else return ()

-- | Log a parser call with the remaining input (saves to log collection)
logParserCall :: String -> String -> IO ()
logParserCall name input = do
  enabled <- isParserDebugEnabled
  if enabled
    then do
      hPutStrLn stderr $ "[PARSER] Call: " ++ name
      hPutStrLn stderr $ "[PARSER] Input: \"" ++ input ++ "\""
    else return ()

-- | Log parser result and remaining input after parsing
logParserResult :: Show a => String -> a -> String -> IO ()
logParserResult name result remainingInput = do
  enabled <- isParserDebugEnabled
  if enabled
    then do
      hPutStrLn stderr $ "[PARSER] Result from " ++ name ++ ": " ++ show result
      hPutStrLn stderr $ "[PARSER] Remaining: \"" ++ remainingInput ++ "\""
      hPutStrLn stderr $ "[PARSER] ------------------"
    else return ()

-- | Print all parser logs 
printParserLogs :: IO ()
printParserLogs = do
  enabled <- isParserDebugEnabled
  if enabled
    then do
      hPutStrLn stderr "=========================================="
      hPutStrLn stderr "[PARSER] End of parsing session"
      hPutStrLn stderr "=========================================="
    else return ()

-- | Execute an action with logging before and after
withLogging :: MonadIO m => Text -> m a -> m a
withLogging name action = do
  logDebug $ (T.pack "Starting ") <> name
  result <- action
  logDebug $ (T.pack "Completed ") <> name
  return result
