module List (perms, collapse) where

import Data.List (find)

-- Generates all permutations of the given list.
perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [(xs !! i) : p | i <- [0 .. length xs - 1], p <- perms (take i xs ++ drop (i + 1) xs)] -- FIXME: this is ugly :(

-- Concatenate the shortest prefix of xss whose total sum is positive.
-- If no sum is positive, then the whole list is concatenated.
collapse :: [[Int]] -> [Int]
collapse xss = concat . take prefixLength $ xss
  where
    initSums = scanl (+) 0 (map sum xss)
    prefixLength = maybe (length xss) fst (find ((> 0) . snd) (zip [0 ..] initSums))
