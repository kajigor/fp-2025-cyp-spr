module Binary where 

-- Checks that the given list is a valid reversed binary representation 
isValid :: [Int] -> Maybe [Int]
isValid xs = undefined 

-- Generates an infinite list of reversed binary representations of natural numbers in order
nums :: [[Int]]
nums = undefined 

-- Computes the reversed binary representation of a positive number: no leading zeros
-- toBinary 6 == Just [0,1,1]
-- toBinary 0 == Just [0]
-- toBinary (-10) == Nothing 
toBinary :: Int -> Maybe [Int]
toBinary n = undefined 

-- Computes the integer from a given reversed binary representation 
-- fromBinary [0,1,1] == Just 6 
-- fromBinary [0] == Just 0 
-- fromBinary [1,2,3] == Nothing 
fromBinary :: [Int] -> Maybe Int
fromBinary xs = undefined 