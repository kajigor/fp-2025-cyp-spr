import Data.List (nub)
import Data.Maybe (fromMaybe, listToMaybe)


data Lambda
    = Var String         -- Variable (e.g., "x")
    | Abs String Lambda  -- Lambda-abstraction (λx.M)
    | App Lambda Lambda  -- Application (M N)
    deriving Eq

instance Show Lambda where
    show (Var x) = x
    show (Abs x body) = "(λ" ++ x ++ "." ++ show body ++ ")"
    show (App f x) = "(" ++ show f ++ show x ++ ")"

simpleSubstitute :: String -> Lambda -> Lambda -> Lambda
simpleSubstitute var new (Var x) = if (x == var) then new else (Var x)
simpleSubstitute var new (App f x) = (App (simpleSubstitute var new f) (simpleSubstitute var new x))
simpleSubstitute var new (Abs x body) = if (x == var) then (Abs x body) else (Abs x (simpleSubstitute var new body))

freeVars :: Lambda -> [String]
freeVars (Var x)       = [x]
freeVars (Abs x body)  = filter (/= x) (freeVars body)
freeVars (App f x)     = nub ((freeVars f) ++ (freeVars x))

renameFree :: String -> String -> Lambda -> Lambda
renameFree old new (Var x)      = if (x == old) then (Var new) else (Var x)
renameFree old new (Abs x body) = if (x == old) then (Abs x body) else (Abs x (renameFree old new body))
renameFree old new (App f x)    = (App (renameFree old new f) (renameFree old new x))

-- Thx ChatGPT for this hack
freshVar :: [String] -> String
freshVar used =
    fromMaybe "v_default" (listToMaybe (filter (`notElem` used) candidates))
  where
    candidates = [ "v" ++ show n | n <- [1..] ]
-- Thx ChatGPT for this hack

smartSubstitute :: String -> Lambda -> Lambda -> Lambda
smartSubstitute var new (Var x) = if (x == var) then new else (Var x)
smartSubstitute var new (App f x) = (App (smartSubstitute var new f) (smartSubstitute var new x))
smartSubstitute var new (Abs x body)
    | x == var = (Abs x body)
    | x `elem` freeVars new =
        let x' = freshVar (freeVars body ++ freeVars new ++ [var])
        in (Abs x' (smartSubstitute var new (renameFree x x' body)))
    | otherwise = (Abs x (smartSubstitute var new body))

class AlphaEq a where
    alphaEq :: a -> a -> Bool

instance AlphaEq Lambda where
    alphaEq (Var x) (Var y) = x == y
    alphaEq (App f1 x1) (App f2 x2) = (alphaEq f1 f2) && (alphaEq x1 x2)
    alphaEq (Abs x body1) (Abs y body2)
        | x == y = (alphaEq body1 body2)
        | otherwise = alphaEq (simpleSubstitute x (Var y) body1) body2
    alphaEq _ _ = False

-- Experiments and tests
-- All output comparisons have to return True

main :: IO ()
main = do

    -- Lambda Terms
    let term1 = (Var "x")
    let term2 = (Abs "x" (Var "x"))
    let term3 = (App (Var "x") (Var "y"))
    let term4 = (Abs "y" (App (Var "x") (Var "y")))

    putStrLn "Lambda Terms:"
    print term1
    print term2
    print term3
    print term4


    -- Testing Equality
    putStrLn "\nTesting Equality:"
    print (term1 == term1)      -- Should be True
    print (not (term2 == term3))    -- Should be True


    -- Testing Substitution
    putStrLn "\nTesting Substitution:"
    let sub1 = simpleSubstitute "x" (Var "z") term1  -- Should replace x with z
    let sub2 = simpleSubstitute "x" (Var "z") term2  -- Should replace x inside λx. x
    let sub3 = simpleSubstitute "x" (Var "z") term3  -- Should replace x in (x y)

    print sub1
    print sub2
    print sub3
    print (simpleSubstitute "x" (Var "y") (Abs "y" (Var "x")))


    -- Testing Substitution with Alpha Conversion
    putStrLn "\nTesting Substitution with Alpha Conversion:"
    print (simpleSubstitute "x" (Var "y") (Abs "y" (Var "x")))  -- Expected to rename the parameter to avoid capture
    print (smartSubstitute "x" (Var "y") (Abs "y" (Var "x")))

    let term5 = (Abs "x" (App (Var "x") (Var "y")))
    print (simpleSubstitute "y" (Var "x") term5)
    print (smartSubstitute "y" (Var "x") term5)

    let term6 = (App (Abs "x" (App (Var "x") (Var "z"))) (Var "x"))
    print (simpleSubstitute "x" (Var "w") term6)
    print (smartSubstitute "x" (Var "w") term6)

    -- More Complex Terms for Substitution and Alpha-Equivalence
    putStrLn "\n=== More Complex Terms ==="
    let comp1 = (App (Abs "x" (Abs "x" (App (Var "x") (Var "y")))) (Var "z"))
    let comp2 = (Abs "x" (App (Abs "y" (Var "x")) (Var "y")))

    putStr "comp1: "
    print comp1
    putStr "comp2: "
    print comp2

    putStrLn "\nSubstituting y := comp2 in comp1 (naive and smart):"
    print (simpleSubstitute "y" comp2 comp1)
    print (smartSubstitute "y" comp2 comp1)

    -- Testing Alpha-Equivalence (alphaEq)
    putStrLn "\n=== Testing Alpha-Equivalence (alphaEq) ==="

    let a1 = (Abs "x" (Var "x"))
    let a2 = (Abs "y" (Var "y"))
    print $ alphaEq a1 a2  -- True, same structure up to renaming

    let a3 = (Abs "x" (App (Var "x") (Var "y")))
    let a4 = (Abs "z" (App (Var "z") (Var "y")))
    print $ alphaEq a3 a4  -- True

    let a5 = (Abs "x" (App (Var "x") (Var "y")))
    let a6 = (Abs "x" (App (Var "x") (Var "x")))
    print $ (not (alphaEq a5 a6))  -- True

    let a7  = (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
    let a8  = (Abs "u" (Abs "v" (App (Var "u") (Var "v"))))
    print $ alphaEq a7 a8  -- True

    let a9  = (Abs "x" (Abs "x" (Var "x")))
    let a10 = (Abs "u" (Abs "v" (Var "v")))
    print $ alphaEq a9 a10 -- True


    -- Demonstration that naive and smart substitution often give the same result
    putStrLn "\n=== Naive vs. Smart Substitution (no capture) ==="
    let subNaive1 = simpleSubstitute "x" (Var "z") term3
    let subSmart1 = smartSubstitute  "x" (Var "z") term3
    putStrLn $ "Naive : " ++ show subNaive1
    putStrLn $ "Smart : " ++ show subSmart1


    -- Example that FORCES Different Results (variable capture)
    putStrLn "\n=== Example That FORCES Different Results ==="
    let comp7 = (Abs "y" (Var "x"))  -- λy.x
    let comp8 = (Var "y")            -- We'll substitute x := y

    putStrLn $ "comp7 = " ++ show comp7
    putStrLn $ "comp8 = " ++ show comp8

    putStrLn "\nNaive Substitution (x := y):"
    print (simpleSubstitute "x" comp8 comp7)
    putStrLn "Smart Substitution (x := y):"
    print (smartSubstitute  "x" comp8 comp7)
    putStrLn "(Notice the difference: the naive version captures the free 'y')."


    -- Additional Checks
    putStrLn "\n=== Additional Checks ==="
    putStrLn $ "Free vars of comp7: " ++ show (freeVars comp7)
    putStrLn $ "Free vars of comp8: " ++ show (freeVars comp8)
    let subNaive2 = simpleSubstitute "y" (Var "z") comp7
    let subSmart2 = smartSubstitute  "y" (Var "z") comp7
    putStrLn $ "Substituting y := z in comp7 (naive): " ++ show subNaive2
    putStrLn $ "Substituting y := z in comp7 (smart) : " ++ show subSmart2

    putStrLn "\nAll done!"
