data Lambda
    = Variable String
    | Function String Lambda   
    | Application Lambda Lambda
    deriving Eq

instance Show Lambda where
    show (Variable x) = x
    show (Function p t) = "(λ" ++ p ++ "." ++ show t ++ ")"
    show (Application t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"

substitution :: Lambda -> String -> Lambda -> Lambda
substitution (Variable name) x y = if name == x then y else Variable name
substitution (Function p term) x y
    | p == x = Function p term                        -- if x isn't free we cannot substitute it
    | otherwise = Function p (substitution term x y)  -- otherwise consider the body
substitution (Application t1 t2) x y = 
    Application (substitution t1 x y) (substitution t2 x y)


class AlphaEq a where
    alphaEq:: a -> a -> Bool


-- To implement alphaEq maintain an environment that establishes a bijection
-- between the bound variables of the first term and the corresponding variables in the second term
instance AlphaEq Lambda where
    alphaEq t1 t2 = alphaEq' [] t1 t2 where
        alphaEq' env (Variable x) (Variable y) = 
            case lookup x env of
                Just y' -> y == y'
                Nothing -> x == y
        alphaEq' env (Function x t) (Function y t') = alphaEq' ((x, y) : env) t t'
        alphaEq' env (Application t1 t2) (Application t1' t2') = alphaEq' env t1 t1' && alphaEq' env t2 t2'
        alphaEq' _ _ _ = False

main :: IO ()
main = do
    let id1 = Function "x" (Variable "x")  -- λx.x
    let id2 = Function "y" (Variable "y")  -- λy.y
    let term1 = Function "x" (Application (Variable "x") (Variable "z"))  --λx.xz
    let term2 = Function "y" (Application (Variable "y") (Variable "z"))  --λy.yz
    let term3 = Function "y" (Application (Variable "x") (Variable "y"))  --λy.xy
    let term4 = Function "x" (Application (Function "x" (Variable "x")) (Variable "x"))  -- λx.(λx.x)x 
    let term5 = Function "x" (Application (Function "y" (Variable "y")) (Variable "x"))  -- λx.(λy.y)x

    -- Print all terms
    print id1
    print id2
    print term1
    print term2
    print term3
    print term4
    print term5
    putStr "\n"

    -- Check AlphaEq
    putStrLn "AlphaEq tests:"
    putStrLn $ "id1 = " ++ show id1
    putStrLn $ "id2 = " ++ show id2
    if alphaEq id1 id2 then putStrLn "Correct" else putStrLn "Incorrect"
    
    putStrLn $ "term1 = " ++ show term1
    putStrLn $ "term2 = " ++ show term2
    if alphaEq term1 term2 then putStrLn "Correct" else putStrLn "Incorrect"

    putStrLn $ "term1 = " ++ show term1
    putStrLn $ "term3 = " ++ show term3
    if not (alphaEq term1 term3) then putStrLn "Correct" else putStrLn "Incorrect"

    putStrLn $ "term4 = " ++ show term4
    putStrLn $ "term5 = " ++ show term5
    if alphaEq term4 term5 then putStrLn "Correct" else putStrLn "Incorrect"
    putStr "\n"
    
    -- Check substitution
    putStrLn "Substitution tests:"
    let subst1 = substitution id1 "x" (Variable "w")
    putStrLn $ "substitution " ++ show id1 ++ " ('x' -> 'w') = " ++ show subst1
    if subst1 == id1 then putStrLn "Correct" else putStrLn "Incorrect"
    let subst2 = substitution term1 "z" (Variable "w")
    putStrLn $ "substitution " ++ show term1 ++ " ('z' -> 'w') = " ++ show subst2
    if subst2 == Function "x" (Application (Variable "x") (Variable "w")) then putStrLn "Correct" else putStrLn "Incorrect"