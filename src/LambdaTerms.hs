module LambdaTerms (LambdaTerm (..), substitute, AlphaEq (..)) where

data LambdaTerm where
  Var :: String -> LambdaTerm
  Abstraction :: String -> LambdaTerm -> LambdaTerm
  Application :: LambdaTerm -> LambdaTerm -> LambdaTerm
  deriving (Eq)

instance Show LambdaTerm where
  show (Var x) = x
  show (Abstraction x t) = "\\" ++ x ++ " -> " ++ show t
  show (Application t1 t2) = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"

substitute :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
substitute x s (Var y)
  | x == y = s
  | otherwise = Var y
substitute x s (Abstraction y t)
  | x == y = Abstraction y t
  | otherwise = Abstraction y (substitute x s t)
substitute x s (Application t1 t2) = Application (substitute x s t1) (substitute x s t2)

class AlphaEq a where
  alphaEq :: a -> a -> Bool

instance AlphaEq LambdaTerm where
  alphaEq (Var x) (Var y) = x == y
  alphaEq (Abstraction x t1) (Abstraction y t2)
    | x == y = alphaEq t1 t2
    | otherwise = alphaEq (substitute x (Var y) t1) (substitute y (Var y) t2)
  alphaEq (Application t1 t2) (Application s1 s2) = alphaEq t1 s1 && alphaEq t2 s2
  alphaEq _ _ = False
