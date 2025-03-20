module Tree (Tree(..)
            , fromList
            , size
            , node
            , toList
            , fetch
            , update
            , wellFormed) 
  where 

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

-- Constructs a perfect tree from a list
fromList :: [a] -> (Tree a, [a])
fromList xs = buildTree (length xs) xs
  where
    buildTree 0 _  = error "Empty list in buildTree"
    buildTree 1 (y:ys) = (Leaf y, ys)
    buildTree n ys = 
      let (leftSize, rightSize) = split n
          (leftTree, rest) = buildTree leftSize ys
          (rightTree, rest') = buildTree rightSize rest
      in (node leftTree rightTree, rest')
    split n = (n `div` 2, n `div` 2 + n `mod` 2)
  
-- >>> fromList "abcd"

-- Flattens a tree into a list 
toList :: Tree a -> [a]
toList (Leaf x)       = [x]
toList (Node _ xl xr) = toList xl ++ toList xr

-- Fetches the k-th element in the tree 
fetch :: Int -> Tree a -> a
fetch k t = toList t !! k

-- Updates the k-th element of a tree 
update :: Show a => Int -> a -> Tree a -> Tree a 
update _ x (Leaf _)        = Leaf x
update k x (Node sz xl xr)  = if size xl <= k 
                               then Node sz xl (update (k - size xl) x xr)
                               else Node sz (update k x xl) xr

-- Checks that the tree is perfect, i.e. it contains exactly 2^n leaves, 
-- and each node has two well-formed children of equal size. 
-- Example of a perfect tree: Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f'))
wellFormed :: Tree a -> Bool 
wellFormed (Leaf _)       = True
wellFormed (Node _ xl xr) = size xl == size xr && wellFormed xl && wellFormed xr
