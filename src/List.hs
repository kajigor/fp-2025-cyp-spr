module List (perms, collapse) where


-- Generates all permutations of the given list. 
perms :: [a] -> [[a]]
perms []  = [[]]
perms xs  = let dropAt :: Int -> [a] -> [a]
                dropAt ind xs' = take ind xs' ++ drop (ind+1) xs' in
    [(xs !! x):xs' | x <- [0..length xs-1], xs' <- perms (dropAt x xs)]

-- Concatenate the shortest prefix of xss whose total sum is positive. 
-- If no sum is positive, then the whole list is concatenated.
collapse :: [[Int]] -> [Int]
collapse xss =
    head ([concat (take n xss) | n <- [1..length xss-1], sum (concat (take n xss)) > 0] ++ [concat xss])

