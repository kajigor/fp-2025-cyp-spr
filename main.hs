import Distribution.Simple.Utils (xargs)
-- -- Variables
type V = Char

-- instance Show V where
--     show (V c) = [c]

-- Lambda
data L
    = Var V    -- Variable
    | App L L  -- Application
    | Abs V L  -- Abstraction
    deriving (Eq) 

instance Show L where
    show (Var v) = [v]
    show (App l r) = "(" ++ show l ++ " " ++ show r ++ ")"
    show (Abs v e) = "(λ" ++ [v] ++ " . " ++ show e ++ ")"

apply :: L -> L
apply (App l r) = undefined
apply l = l

lam1 = App (Var 'x') (Var 'y')
lam2 = App (Abs 'x' lam1) (Var 'y')

-- (x y (λx y . x z (w x) y))
-- (((x y) (λx . (λy . (((x z) (w x)) y)))))
lam3 = App (App x y) $ Abs 'x' $ Abs 'y' $ App (App (App x z) (App w x)) y
    where
        x = Var 'x'
        y = Var 'y'
        z = Var 'z'
        w = Var 'w'

-- [y ↦ w (λx. w x)]
sub1 = App w $ Abs 'x' $ App w x
    where
        x = Var 'x'
        w = Var 'w'

res1 :: String
res1 = "((x (w (λx . (w x)))) (λx . (λy . (((x z) (w x)) y))))"

substitute :: L -> V -> L -> L
substitute n x m = case m of
    Var v -> if x == v then n else Var v
    App l r -> App (sub l) (sub r)
    Abs v e | v == x    -> Abs v e
    Abs v e | otherwise -> Abs v $ sub e
    where sub = substitute n x

main = do
    print lam1
    print lam2
    print lam3
    print sub1
    -- [y ↦ w (λx. w x)] (x y (λx y. x z (w x) y))
    print $ substitute sub1 'y' lam3
    print res1
    print $ show (substitute sub1 'y' lam3) == res1
    

