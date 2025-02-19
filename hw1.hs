module Main where

    data Lambda
        = Var String         
        | Fun String Lambda 
        | App Lambda Lambda  -- application
        deriving (Eq)

    instance Show Lambda where
      show (Var x)     = x
      show (Fun x b)   = "\\" ++ x ++ ". " ++ show b
      show (App f a)   = "(" ++ show f ++ " " ++ show a ++ ")"


    substitute :: Lambda -> String -> Lambda -> Lambda
    substitute (Var y) x newExpr =
        if x == y
            then newExpr
            else Var y
    substitute (Fun y b) x newExpr =
        if x == y
            then Fun y b
            else Fun y (substitute b x newExpr)
    substitute (App f a) x newExpr =
        App (substitute f x newExpr) (substitute a x newExpr)


    class AlphaEq a where
        alphaEq :: a -> a -> Bool

    instance AlphaEq Lambda where
        alphaEq (Var v1)      (Var v2)      = v1 == v2
        alphaEq (App f1 a1)   (App f2 a2)   = alphaEq f1 f2 && alphaEq a1 a2
        alphaEq (Fun x b1)    (Fun y b2)
            | x == y    = alphaEq b1 b2
            | otherwise = alphaEq b1 (substitute b2 y (Var x))
        alphaEq _ _ = False


    main :: IO ()
    main = do
        let idX    = Fun "x" (Var "x")                  -- \x. x
        let idY    = Fun "y" (Var "y")                  -- \y. y
        let constX = Fun "x" (Fun "y" (Var "x"))        -- \x. (\y. x)
        let ex1    = App idX (Var "z")                  -- (\x. x) z
        let ex2    = App (Fun "x" (App (Var "x") (Var "x"))) (Var "y") --(\x. x x) y

        putStrLn ("idX = " ++ show idX) --expr
        putStrLn ("idY = " ++ show idY)
        putStrLn ("constX = " ++ show constX)
        putStrLn ("ex1 = " ++ show ex1)
        putStrLn ("ex2 = " ++ show ex2)

        putStrLn ("idX ≡ idY: " ++ show (alphaEq idX idY)) --id = id
        putStrLn ("idX ≡ constX: " ++ show (alphaEq idX constX)) --falce(id \x. x != const \x. (\y. x))
        putStrLn ("constX ≡ Fun \"a\" (Fun \"b\" (Var \"a\")): " ++ 
            show (alphaEq constX (Fun "a" (Fun "b" (Var "a"))))) --should be true obviously

        let notEq1 = Fun "x" (App (Var "x") (Var "y")) -- \x. (x y)
        let notEq2 = Fun "x" (App (Var "x") (Var "z")) -- \x. (x z)
        putStrLn ("notEq1 ≡ notEq2: " ++ show (alphaEq notEq1 notEq2)) --false

        let sub1 = substitute (Var "x") "x" (Var "z") -- x[x := z] => z
        putStrLn ("Var \"x\"[x := z] = " ++ show sub1) -- should be z

        let sub2 = substitute (Fun "x" (Var "x")) "x" (Var "y") -- (\x. x)[x := y] => \x. x
        putStrLn ("Fun \"x\" (Var \"x\")[x := y] = " ++ show sub2) --should be \x. x

        let sub3 = substitute (App (Var "x") (Var "y")) "y" (Var "z") -- (x y)[y := z] => (x z)
        putStrLn ("App (Var \"x\") (Var \"y\")[y := z] = " ++ show sub3) -- should be (x z)

        let sub4 = substitute (Fun "x" (App (Var "x") (Var "y"))) "y" (Var "z") -- (\x. (x y))[y := z] => \x. (x z)
        putStrLn ("Fun \"x\" (App (Var \"x\") (Var \"y\"))[y := z] = " ++ show sub4) --shoud be\x. (x z)

        let sub5 = substitute (Var "a") "x" (Var "z") -- no substitution
        putStrLn ("Var \"a\"[x := z] = " ++ show sub5) --a

