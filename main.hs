module Main where

data Lambda = Var String
           | Abs String Lambda
           | App Lambda Lambda
           deriving (Eq)

instance Show Lambda where
    show (Var x) = x
    show (Abs x body) = "\\" ++ x ++ " -> " ++ show body
    show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

substitute :: String -> Lambda -> Lambda -> Lambda
substitute x replacement (Var y) | x == y = replacement
                                 | otherwise = Var y
substitute x replacement (Abs y body)
    | x == y = Abs y body
    | otherwise = Abs y (substitute x replacement body)
substitute x replacement (App t1 t2) = App (substitute x replacement t1) (substitute x replacement t2)

class AlphaEq a where
    alphaEq :: a -> a -> Bool

instance AlphaEq Lambda where
    alphaEq (Var x) (Var y) = x == y
    alphaEq (App t1 t2) (App t3 t4) = alphaEq t1 t3 && alphaEq t2 t4
    alphaEq (Abs x body1) (Abs y body2) = alphaEq (substitute x (Var y) body1) body2
    alphaEq _ _ = False

main :: IO ()
main = do
    let term1 = Abs "x" (Var "x")
    let term2 = Abs "y" (Var "y")
    let term3 = Abs "x" (App (Var "x") (Var "y"))
    let term4 = Abs "z" (App (Var "z") (Var "y"))
    
    putStrLn "Lambda terms:"
    print term1
    print term2
    print term3
    print term4
    
    putStrLn "\nSubstitution examples:"
    print $ substitute "x" (Var "z") term3
    
    putStrLn "\nAlpha equivalence checks:"
    print $ alphaEq term1 term2 -- True
    print $ alphaEq term3 term4 -- True
    print $ alphaEq term1 term3 -- False
