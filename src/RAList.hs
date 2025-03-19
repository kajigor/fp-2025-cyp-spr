module RAList where 

import qualified Tree as T
import Data.Maybe (fromJust)
import Binary (toBinary)

-- This is an implementation of a random-access list. 
-- It's possible to achieve logarithmic access by representing a list as a sequence of perfect trees. 
-- The sequence should correspond to the reversed binary representation of the size of the list. 
-- For example, the list ['a'..'f'] of size 6 (011) corresponds to the following sequence: 
-- [ Zero
-- , One (Node 2 (Leaf 'a') (Leaf 'b'))
-- , One (Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f')))
-- ]
-- Any operations over the random-access list should preserve its well-formedness. 

-- Encodes "bits" in the sequence
data Digit a 
  = Zero 
  | One (T.Tree a)
  deriving (Show, Eq)

-- Encodes a list as the sequence of perfect trees 
type RAList a = [Digit a]

-- Checks that the random-access list has correct structure
wellFormed :: RAList a -> Bool
wellFormed [] = True
wellFormed (Zero:xs) = wellFormed xs
wellFormed (One t:xs) = T.wellFormed t && wellFormed xs 

-- Flattens a random-access list into a normal list
toList :: RAList a -> [a] 
toList [] = []
toList (Zero:xs) = toList xs
toList (One t:xs) = T.toList t ++ toList xs 

-- Generates a random-access list from a normal list 
fromList :: [a] -> RAList a 
fromList [] = []
fromList [x] = [One (T.Leaf x)]
fromList xs = let lastTreeSize = 2 ^ ((length . fromJust . toBinary . length) xs - 1) 
  in fromList (take (length xs - lastTreeSize) xs) ++ [One (fromWellFormedList (drop (length xs - lastTreeSize) xs))] where
    fromWellFormedList :: [a] -> T.Tree a
    fromWellFormedList [x] = T.Leaf x
    fromWellFormedList xs' = T.node (fromWellFormedList (take (length xs' `div` 2) xs')) (fromWellFormedList (drop (length xs' `div` 2) xs'))


-- Fetches the k-th element of the list 
fetch :: Int -> RAList a -> a 
fetch _ [] = error "Index out of bounds"
fetch k (Zero:xs) = fetch k xs
fetch k (One t:xs) 
  | k < T.size t = T.fetch k t
  | otherwise = fetch (k - T.size t) xs 

-- Checks the list for emptiness 
null :: RAList a -> Bool 
null xs = length xs == 0

-- Creates the empty list 
nil :: RAList a
nil = []  

-- Adds a given element to the head of the list. 
-- Should always return a well-formed list. 
cons :: a -> RAList a -> RAList a 
cons h t = fromList [h] ++ t

-- Splits the list into a head and a tail, if not empty. 
-- The resulting tail should be a well-formed list. 
uncons :: RAList a -> Maybe (a, RAList a) 
uncons xs =
  case toList xs of
    []     -> Nothing
    (h:t)  -> Just (h, fromList t)

-- Updates the k-th element of the list with the given value 
update :: Show a => Int -> a -> RAList a -> RAList a
update _ _ [] = error "Index out of bounds"
update k v (Zero:xs) = Zero : update k v xs
update k v (One t:xs) 
  | k < T.size t = One (T.update k v t) : xs
  | otherwise = One t : update (k - T.size t) v xs