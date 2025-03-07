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
toList t = undefined 

-- Fetches the k-th element in the tree 
fetch :: Int -> Tree a -> a
fetch k t = undefined 

-- Updates the k-th element of a tree 
update :: Show a => Int -> a -> Tree a -> Tree a 
update k x t = undefined 

-- Checks that the tree is perfect, i.e. it contains exactly 2^n leaves, 
-- and each node has two well-formed children of equal size. 
-- Example of a perfect tree: Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f'))
wellFormed :: Tree a -> Bool 
wellFormed t = undefined 