module Tree (Tree (..), size, node, toList, fetch, update, wellFormed) where

-- A tree data structure with explisit size parameter in the node
data Tree a
  = Leaf a
  | Node Int (Tree a) (Tree a)
  deriving (Show, Eq)

size :: Tree a -> Int
size (Leaf _) = 1
size (Node n _ _) = n

-- Smart constructor to create a node
node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

-- Flattens a tree into a list
toList :: Tree a -> [a]
toList (Leaf a) = [a]
toList (Node _ l r) = toList l ++ toList r

-- Fetches the k-th element in the tree
fetch :: Int -> Tree a -> a
fetch _ (Leaf a) = a
fetch k (Node _ l r)
  | k < size l = fetch k l
  | otherwise = fetch (k - size l) r

-- Updates the k-th element of a tree
update :: Int -> a -> Tree a -> Tree a
update _ x (Leaf _) = Leaf x
update k x (Node s l r)
  | k < size l = Node s (update k x l) r
  | otherwise = Node s l (update (k - size l) x r)

-- Checks that the tree is perfect, i.e. it contains exactly 2^n leaves,
-- and each node has two well-formed children of equal size.
-- Example of a perfect tree: Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f'))
wellFormed :: Tree a -> Bool
wellFormed (Leaf _) = True
wellFormed (Node n l r) = n == lsize + rsize && lsize == rsize && wellFormed l && wellFormed r
  where
    (lsize, rsize) = (size l, size r)
