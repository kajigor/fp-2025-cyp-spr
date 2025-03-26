module Expr where 
  
import qualified Data.Map as M 
import Control.Monad.State

-- TODO: Extend the language with subtraction, multiplication, and division
data Expr 
  = Variable String
  | Constant Int
  | Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- Substitution which maps known variables to their assigned values 
type Subst = M.Map String Int 

-- TODO: Extend the interpreter to support additional contructs 
eval :: Expr -> Subst -> Int 
eval (Variable var) s = s M.! var 
eval (Constant const) _ = const 
eval (Plus x y) s = eval x s + eval y s
eval (Minus x y) s = eval x s - eval y s
eval (Times x y) s = eval x s * eval y s
eval (Div x y) s = eval x s `div` eval y s

-- TODO: Extend the language with a while-loop and a read statement. 
-- Interpret 0 as False in a condition of a while-loop.
-- Read statement reads an integer value from the input and assigns it to its argument-variable. 
-- Model the input as a list of integers, which will involve modifying the notion of the state. 
data Stmt
  = Assign String Expr
  | Seq Stmt Stmt
  | Write Expr
  | While Expr Stmt
  | Read String

-- TODO: Extend the interpreter to support while-loops and read-statements.
evalStmt (Assign var e) = do
  (subst, input) <- get
  let e' = eval e subst
  put (M.insert var e' subst, input)
  return []

evalStmt (Seq s1 s2) = do
  out1 <- evalStmt s1
  out2 <- evalStmt s2
  return (out1 ++ out2)

evalStmt (Write e) = do
  (subst, input) <- get
  return [eval e subst]

evalStmt (Read var) = do
  (subst, input) <- get
  case input of
    []     -> error "No more input values!"
    (x:xs) -> do
      put (M.insert var x subst, xs)
      return []

evalStmt (While cond body) = do
  (subst, input) <- get
  if eval cond subst == 0
    then return []
    else do
      put (subst, input)
      out1 <- evalStmt body
      out2 <- evalStmt (While cond body)
      return (out1 ++ out2)




program = 
  Assign "x" (Constant 13)
  `Seq` Assign "y" (Plus (Variable "x") (Variable "x"))
  `Seq` Write (Constant 777) 
  `Seq` Write (Variable "y")
  `Seq` Write (Variable "x") 
  `Seq` Assign "x" (Plus (Variable "y") (Constant 10))
  `Seq` Write (Variable "x")

program2 =
  Read "x" `Seq`
  Assign "sum" (Constant 0) `Seq`
  While (Variable "x") (
    Assign "sum" (Plus (Variable "sum") (Variable "x")) `Seq`
    Assign "x" (Minus (Variable "x") (Constant 1))
  ) `Seq`
  Write (Variable "sum")


program3 =
  Assign "a" (Constant 5)
  `Seq` Assign "b" (Constant 10)
  `Seq` Assign "c" (Plus (Variable "a") (Variable "b"))
  `Seq` Write (Variable "c")

program4 =
  Assign "x" (Div (Constant 20) (Constant 4)) `Seq`
  Write (Variable "x")

program5 :: Stmt
program5 =
  Read "x" `Seq` Write (Times (Variable "x") (Variable "x"))


fibProgram =
  Read "n"
  `Seq` Assign "a" (Constant 0)
  `Seq` Assign "b" (Constant 1)
  `Seq` Assign "i" (Constant 1)
  `Seq` While (Minus (Variable "n") (Variable "i")) (
    Assign "next" (Plus (Variable "a") (Variable "b")) `Seq`
    Assign "a" (Variable "b") `Seq`
    Assign "b" (Variable "next") `Seq`
    Assign "i" (Plus (Variable "i") (Constant 1))
  )
  `Seq` Write (Variable "b")


runProgram :: Stmt -> [Int] -> [Int]
runProgram p input = evalState (evalStmt p) (M.empty, input)


main :: IO ()
main = do
  print $ runProgram program [] -- [777,26,13,36]

  print $ runProgram program2 [4] -- 10
  print $ runProgram program3 [] -- 15
  print $ runProgram program4 [] -- 5

  print $ runProgram program5 [5] -- 25
  print $ runProgram program5 [7] -- 49
  print $ runProgram program5 [9] -- 81

  print $ runProgram fibProgram [4] -- 3
  print $ runProgram fibProgram [5] -- 5
  print $ runProgram fibProgram [7] -- 13
  print $ runProgram fibProgram [20] -- 6765




