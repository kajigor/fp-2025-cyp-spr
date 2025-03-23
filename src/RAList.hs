module RAList (Digit (..), RAList, wellFormed, toList, fromList, fetch, RAList.null, nil, cons, uncons, update) where

import qualified Tree as T

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
wellFormed = all wellFormedDigit
  where
    wellFormedDigit Zero = True
    wellFormedDigit (One t) = T.wellFormed t

-- Flattens a random-access list into a normal list
toList :: RAList a -> [a]
toList = concatMap digitToList
  where
    digitToList Zero = []
    digitToList (One t) = T.toList t

-- Generates a random-access list from a normal list
fromList :: [a] -> RAList a
fromList = foldr cons nil

-- Fetches the k-th element of the list
fetch :: Int -> RAList a -> a
fetch _ [] = error "Index out of bounds"
fetch k (Zero : rest) = fetch k rest
fetch k (One t : rest)
  | k < ts = T.fetch k t
  | otherwise = fetch (k - ts) rest
  where
    ts = T.size t

-- Checks the list for emptiness
null :: RAList a -> Bool
null [] = True
null _ = False

-- Creates the empty list
nil :: RAList a
nil = []

-- Adds a tree to the head of the list. It's assumed that the tree is of size 2^k, where k is the iteration.
consTree :: T.Tree a -> RAList a -> RAList a
consTree tree [] = [One tree]
consTree tree (Zero : rest) = One tree : rest
consTree tree (One t' : rest) = Zero : consTree (T.Node (T.size tree + T.size t') tree t') rest

-- Adds a given element to the head of the list.
-- Should always return a well-formed list.
cons :: a -> RAList a -> RAList a
cons h [] = [One (T.Leaf h)]
cons h (Zero : xs) = One (T.Leaf h) : xs
cons h (One t : xs) = Zero : consTree (T.Node 2 (T.Leaf h) t) xs

splitPerfectTree :: T.Tree a -> [T.Tree a]
splitPerfectTree (T.Node _ l r) = r : splitPerfectTree l
splitPerfectTree x = [x]

-- Splits the list into a head and a tail, if not empty.
-- The resulting tail should be a well-formed list.
uncons :: RAList a -> Maybe (a, RAList a)
uncons [] = Nothing
uncons (Zero : xs) = uncons xs
uncons (One t : xs) = Just (h, map One rest ++ xs)
  where
    ((T.Leaf h) : rest) = reverse $ splitPerfectTree t

-- Updates the k-th element of the list with the given value
update :: Int -> a -> RAList a -> RAList a
update _ _ [] = []
update n x (Zero : rest) = Zero : update n x rest
update n x (One t : rest)
  | n < ts = One (T.update n x t) : rest
  | otherwise = One t : update (n - ts) x rest
  where
    ts = T.size t
