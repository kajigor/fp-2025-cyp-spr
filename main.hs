module Main where

data LambdaTerm = 
    Variable String 
    | Abstraction String LambdaTerm 
    | Application LambdaTerm LambdaTerm

instance Show LambdaTerm where
    show (Variable x) = x
    show (Abstraction x expr) = "(λ" ++ x ++ "." ++ show expr ++ ")"
    show (Application t1 t2) = show t1 ++ "(" ++ show t2 ++ ")"

instance Eq LambdaTerm where
    (==) = alphaEq

substitute :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
substitute x x' (Variable z) 
    | x == z = x'
substitute x x' (Abstraction z expr) 
    | x /= z = Abstraction z (substitute x x' expr)
substitute x x' (Application t1 t2) = 
    Application (substitute x x' t1) (substitute x x' t2)
substitute _ _ expr = expr

class Alpha a where
    alphaEq :: a -> a -> Bool

instance Alpha LambdaTerm where
    alphaEq t1 t2 = alphaEq' t1 t2 []
      where
        alphaEq' :: LambdaTerm -> LambdaTerm -> [(String, String)] -> Bool
        alphaEq' (Variable x) (Variable y) env =
            case lookup x env of
                Just y' -> y == y' 
                Nothing -> x == y
        alphaEq' (Abstraction x e1) (Abstraction y e2) env =
            alphaEq' e1 e2 ((x, y) : env)
        alphaEq' (Application t1 t2) (Application t1' t2') env =
            alphaEq' t1 t1' env && alphaEq' t2 t2' env
        alphaEq' _ _ _ = False

main = do
    print "show:"
    let term1 = Abstraction "x" (Variable "x")
        term2 = Abstraction "y" (Variable "y")
        term3 = Abstraction "x" (Variable "y")
    print term1
    print term2
    print term3

    print "substitution"
    let term = Application 
                (Abstraction "x" (Application (Variable "x") (Variable "y")))
                (Variable "z")
    print term
    print $ substitute "y" (Variable "w") term
    print $ substitute "x" (Variable "q") term

    print "alpha"
    putStrLn $ "term1 alpha-equivalent term2: " ++ show (alphaEq term1 term2)
    putStrLn $ "term1 alpha-equivalent term3: " ++ show (alphaEq term1 term3)


