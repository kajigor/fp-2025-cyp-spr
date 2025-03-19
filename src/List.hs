module List where

-- Generates all permutations of the given list. 
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = perms xs >>= \p -> map (\i -> take i p ++ [x] ++ drop i p) [0 .. length p]


-- Concatenate the shortest prefix of xss whose total sum is positive. 
-- If no sum is positive, then the whole list is concatenated.
collapse :: [[Int]] -> [Int]
collapse [] = []
collapse [x] = x
collapse (x:xs) = if sum x > 0 then x else collapse ((x ++ head xs): drop 1 xs)