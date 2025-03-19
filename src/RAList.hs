module RAList where 

import qualified Tree as T
import qualified Binary as B

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
wellFormed []         = True
wellFormed (Zero:xs)  = wellFormed xs
wellFormed (One t:xs) = T.wellFormed t && wellFormed xs

-- Flattens a random-access list into a normal list
toList :: RAList a -> [a] 
toList []         = []
toList (Zero:xs)  = toList xs
toList (One t:xs) = T.toList t ++ toList xs

-- >>> toList [ Zero, One (T.Node 2 (T.Leaf 'a') (T.Leaf 'b')), One (T.Node 4 (T.Node 2 (T.Leaf 'c') (T.Leaf 'd')) (T.Node 2 (T.Leaf 'e') (T.Leaf 'f')))]
-- "abcdef"

-- Generates a random-access list from a normal list 
fromList :: [a] -> RAList a
fromList [] = [Zero]
fromList xs = reverse $ go xs []
  where
    go [] acc = acc
    go ys acc =
      let (tree, rest) = T.fromList ys
      in go rest (One tree : acc)


-- >>> fromList "abcdef"
-- [One (Node 6 (Node 3 (Leaf 'a') (Node 2 (Leaf 'b') (Leaf 'c'))) (Node 3 (Leaf 'd') (Node 2 (Leaf 'e') (Leaf 'f'))))]

-- Fetches the k-th element of the list 
fetch :: Int -> RAList a -> a 
fetch _ []         = undefined
fetch k [One t]    = T.fetch k t
fetch k (Zero:xs)  = fetch k xs
fetch k (One t:xs) = if T.size t > k then T.fetch k t else fetch (k - T.size t) xs

-- Checks the list for emptiness 
null :: RAList a -> Bool 
null [Zero] = True
null _      = False

-- Creates the empty list 
nil :: RAList a
nil = [Zero]

-- Adds a given element to the head of the list. 
-- Should always return a well-formed list. 
cons :: a -> RAList a -> RAList a 
cons h t = undefined

-- Splits the list into a head and a tail, if not empty. 
-- The resulting tail should be a well-formed list. 
uncons :: RAList a -> Maybe (a, RAList a) 
uncons xs = undefined 

-- Updates the k-th element of the list with the given value 
update :: Int -> a -> RAList a -> RAList a
update n x xs = undefined 
