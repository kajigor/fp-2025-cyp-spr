module Tree where 

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
toList (Leaf x) = [x]
toList (Node _ t1 t2) = toList t1 ++ toList t2

-- Fetches the k-th element in the tree 
fetch :: Int -> Tree a -> a
fetch _ (Leaf x) = x
fetch k (Node _ t1 t2) 
  | k < size t1 = fetch k t1
  | otherwise = fetch (k - size t1) t2

-- Updates the k-th element of a tree 
update :: Show a => Int -> a -> Tree a -> Tree a 
update _ x (Leaf _) = Leaf x
update k x (Node _ t1 t2) 
  | k < size t1 = node (update k x t1) t2
  | otherwise = node t1 (update (k - size t1) x t2)

-- Checks that the tree is perfect, i.e. it contains exactly 2^n leaves, 
-- and each node has two well-formed children of equal size. 
-- Example of a perfect tree: Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f'))
wellFormed :: Tree a -> Bool 
wellFormed (Leaf _) = True
wellFormed (Node _ t1 t2) = size t1 == size t2 && wellFormed t1 && wellFormed t2