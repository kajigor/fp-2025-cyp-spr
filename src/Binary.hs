module Binary (isValid, nums, toBinary, fromBinary) where

import Data.Maybe (fromJust)

-- Checks that the given list is a valid reversed binary representation 
isValid :: [Int] -> Maybe [Int]
isValid [] = Nothing
isValid xs = if last xs == 0 || any (\x -> x /= 0 && x /= 1) xs then Nothing else Just xs

-- Generates an infinite list of reversed binary representations of natural numbers in order
nums :: [[Int]]
nums = map (fromJust . toBinary) [0..]

-- Computes the reversed binary representation of a positive number: no leading zeros
-- toBinary 6 == Just [0,1,1]
-- toBinary 0 == Just [0]
-- toBinary (-10) == Nothing 
toBinary :: Int -> Maybe [Int]
toBinary n
  | n < 0 = Nothing
  | n < 2 = Just [n]
  | otherwise = Just $ (n `mod` 2) : fromJust (toBinary (n `div` 2))

index :: Eq a => a -> [a] -> Maybe Int
index _ [] = Nothing
index x xs = if head xs == x then Just 0 else index x (tail xs) >>= (Just . (+1))

-- Computes the integer from a given reversed binary representation 
-- fromBinary [0,1,1] == Just 6 
-- fromBinary [0] == Just 0 
-- fromBinary [1,2,3] == Nothing 
fromBinary :: [Int] -> Maybe Int
fromBinary xs = if all (\x -> x == 0 || x == 1) xs && (last xs == 1 || length xs == 1) then Just (fromJust $ index xs nums) else Nothing

