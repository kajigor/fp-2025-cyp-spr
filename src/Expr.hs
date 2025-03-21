module Expr where 
  
import qualified Data.Map as M 
import Control.Monad.State

-- TODO: Extend the language with subtraction, multiplication, and division
data Expr 
  = V String | C Int | Plus Expr Expr

-- Substitution which maps known variables to their assigned values 
type Subst = M.Map String Int 

-- TODO: Extend the interpreter to support additional contructs 
eval :: Expr -> Subst -> Int 
eval (V v) s = s M.! v 
eval (C n) _ = n 
eval (Plus x y) s = eval x s + eval y s

-- TODO: Extend the language with a while-loop and a read statement. 
-- Interpret 0 as False in a condition of a while-loop.
-- Read statement reads an integer value from the input and assigns it to its argument-variable. 
-- Model the input as a list of integers, which will involve modifying the notion of the state. 
data Stmt 
  = Assign String Expr 
  | Seq Stmt Stmt 
  | Write Expr 

-- TODO: Extend the interpreter to support while-loops and read-statements. 
evalStmt :: Stmt -> State Subst [Int]
evalStmt (Assign v e) = do 
  subst <- get 
  let e' = eval e subst 
  modify (M.insert v e')
  return [] 
evalStmt (Seq s1 s2) = do 
  s1' <- evalStmt s1
  s2' <- evalStmt s2
  return (s1' ++ s2')
evalStmt (Write e) = do 
  subst <- get 
  return [eval e subst]

program = 
  Assign "x" (C 13)
  `Seq` Assign "y" (Plus (V "x") (V "x"))
  `Seq` Write (C 777) 
  `Seq` Write (V "y")
  `Seq` Write (V "x") 
  `Seq` Assign "x" (Plus (V "y") (C 10))
  `Seq` Write (V "x")

runProgram p = 
  execState (evalStmt p) M.empty 
