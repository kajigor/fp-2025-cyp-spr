{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Md2HtmlParser.Metrics
where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (ParsecT)
import Text.Megaparsec.Char (char)
import Control.Monad.State.Strict
import Data.Text (Text)

{-# NOINLINE charCallCounter #-}

type Parser = ParsecT String Text (State Metrics)

data Metrics where
  Metrics :: {charCalls :: !Int} -> Metrics
  deriving (Show)

emptyMetrics :: Metrics
emptyMetrics = Metrics 0


charCallCounter :: IORef Int
charCallCounter = unsafePerformIO $ newIORef 0

-- | Resets the char call counter to zero.
-- Should be called before starting a new parsing operation for which metrics are desired.
resetCharCallCounter :: IO ()
resetCharCallCounter = atomicWriteIORef charCallCounter 0

-- | Retrieves the current value of the char call counter.
getCharCallCount :: IO Int
getCharCallCount = readIORef charCallCounter

-- | Retrieves the current value and then resets the counter.
getAndResetCharCallCount :: IO Int
getAndResetCharCallCount = atomicModifyIORef' charCallCounter (\count -> (0, count))

--charWithCount :: (MonadParsec e s m, Token s ~ Char)
--              => Char -> m (Token s)
charWithCount :: Char -> Parser Char
charWithCount c = do
  modify' (\m -> m { charCalls = charCalls m + 1 })
  char c

incByN :: Int -> a -> Parser a
incByN n x = do
  modify' (\m -> m { charCalls = charCalls m + n })
  return $! x