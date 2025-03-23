module Binary (isValid, nums, toBinary, fromBinary) where

import Control.Monad (guard, (>=>))
import Data.Functor (($>))
import Data.Maybe (fromJust, listToMaybe)

-- Checks that the given list is a valid reversed binary representation
isValid :: [Int] -> Maybe [Int]
isValid = (guard . liftA2 (&&) (all (`elem` [0, 1])) (liftA2 (||) ((== Just 1) . listToMaybe . reverse) (== [0]))) >>= ($>)

-- Generates an infinite list of reversed binary representations of natural numbers in order
nums :: [[Int]]
nums = map (fromJust . toBinary) [0 ..]

-- Computes the reversed binary representation of a positive number: no leading zeros
-- toBinary 6 == Just [0,1,1]
-- toBinary 0 == Just [0]
-- toBinary (-10) == Nothing
toBinary :: Int -> Maybe [Int]
toBinary 0 = Just [0]
toBinary n
  | n < 0 = Nothing
  | otherwise = Just . reverse . dropWhile (== 0) . reverse $ go n
  where
    go 0 = []
    go x = x `mod` 2 : go (x `div` 2)

-- Computes the integer from a given reversed binary representation
-- fromBinary [0,1,1] == Just 6
-- fromBinary [0] == Just 0
-- fromBinary [1,2,3] == Nothing
fromBinary :: [Int] -> Maybe Int
fromBinary = isValid >=> (pure . sum . zipWith (*) (iterate (* 2) 1))
