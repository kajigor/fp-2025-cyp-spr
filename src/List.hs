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
collapse (x:xs) = collapseWithSum x (sum x) xs
    where
        collapseWithSum res _ [] = res
        collapseWithSum res currentSum (y:ys) = if currentSum > 0 then res else collapseWithSum (res ++ y) (currentSum + sum y) ys
