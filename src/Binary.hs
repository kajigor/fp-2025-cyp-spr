module Binary where 

import Data.Maybe (fromJust)
import Control.Monad (guard)

isValidDigit :: Int -> Bool
isValidDigit x = x == 0 || x == 1

-- Checks that the given list is a valid reversed binary representation 
isValid :: [Int] -> Maybe [Int]
isValid [] = Nothing
isValid [_, 0] = Nothing
isValid [x] = do
    guard (isValidDigit x)
    return [x]
isValid (x:xs) = do
    guard (isValidDigit x)
    rest <- isValid xs
    return (x:rest) 

-- Generates an infinite list of reversed binary representations of natural numbers in order
nums :: [[Int]]
nums = map (fromJust . toBinary) [0..]

-- Computes the reversed binary representation of a positive number: no leading zeros
-- toBinary 6 == Just [0,1,1]
-- toBinary 0 == Just [0]
-- toBinary (-10) == Nothing 
toBinary :: Int -> Maybe [Int]
toBinary 0 = Just [0]
toBinary 1 = Just [1]
toBinary n = do
    guard (n > 0)
    rest <- toBinary (n `div` 2)
    return ((n `mod` 2):rest)

-- Computes the integer from a given reversed binary representation 
-- fromBinary [0,1,1] == Just 6 
-- fromBinary [0] == Just 0 
-- fromBinary [1,2,3] == Nothing 
fromBinary :: [Int] -> Maybe Int
fromBinary [] = Nothing
fromBinary [x] = do
    guard (isValidDigit x)
    return x
fromBinary (x:xs) = do
    guard (isValidDigit x)
    rest <- fromBinary xs
    return (x + 2 * rest)


