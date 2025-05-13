module Interpreter (runProgram) where

import Parser
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

type Env = M.Map String Int

data Turtle = Turtle 
  { pos :: (Int, Int)
  , dir :: Int -- direction (0 -> up, 1 -> right, 2 -> down, 3 -> left)
  } deriving (Show)

type Canvas = M.Map (Int, Int) Char -- final position of turtle will be % 
                                    -- and pen will be #, empty points *

data World = World
  { env    :: Env
  , turtle :: Turtle
  , canvas :: Canvas
  , fsize  :: (Int, Int)
  , pen    :: Bool 
  } deriving (Show)

getExpr :: Env -> Expr -> Maybe Int
getExpr _ (Val n) = Just n
getExpr e (Var x) = M.lookup x e

eval :: World -> Command -> Either String World
eval w@(World e (Turtle (x, y) d) canv (wMax, hMax) penPos) (Forward expr) = do
  dist <- case getExpr e expr of
    Just n  -> Right n
    Nothing -> Left "variable isn't defined" 

  let (dx, dy) = case d `mod` 4 of
                  0 -> ( 0,  1)  -- up
                  1 -> ( 1,  0)  -- right
                  2 -> ( 0, -1)  -- down
                  3 -> (-1,  0)  -- left
                  _ -> error "How is this possible..."
      stepIfValid (cx, cy)
        | cx >= 0 && cx < wMax && cy >= 0 && cy < hMax = Just (cx, cy)
        | otherwise = Nothing

      walk 0 curr acc = (curr, acc)
      walk n curr acc =
        case stepIfValid (curr `add` (dx, dy)) of
          Just next ->
            let acc' = if penPos then M.insert next '#' acc else acc
            in walk (n - 1) next acc'
          Nothing -> (curr, acc)  -- if stuck - stand on place

      add (a, b) (a', b') = (a + a', b + b')

      (endPos, canv') = walk dist (x, y) canv

  Right w { turtle = Turtle endPos d, canvas = canv' }
eval w TurnRight = 
  let new_dir = ((dir . turtle $ w) + 1) `mod` 4
      new_turtle = (turtle w) { dir = new_dir }
  in 
    Right w { turtle = new_turtle }
eval w TurnLeft = 
  let new_dir = ((dir . turtle $ w) + 3) `mod` 4
      new_turtle = (turtle w) { dir = new_dir }
  in 
    Right w { turtle = new_turtle }
eval w PenUp   = Right w { pen = False }
eval w PenDown = Right w { pen = True }
eval w (Let name expr) = do
  let e = env w
  case getExpr e expr of 
    Just n -> 
      Right w { env = M.insert name n e }
    Nothing -> Left "undefined variable"
eval w (Repeat times cmds) = 
  case getExpr (env w) times of
    Nothing -> Left "undefined variable"
    Just n  -> foldM eval w (concat . replicate n $ cmds) 

render :: (Int, Int) -> Canvas -> String
render (nRows, nCols) c = unlines
  [ [ fromMaybe '.' (M.lookup (row, col) c)
    | col <- [0 .. nCols - 1] ]
  | row <- [0 .. nRows - 1]
  ]

runProgram :: (Int, Int) -> [Command] -> Either String String
runProgram size cmds = 
  let world = World {
                env = M.empty
              , turtle = Turtle {
                  pos = (0, 0)
                , dir = 0
                }
              , canvas = M.empty
              , fsize = size
              , pen = False
              }
  in 
    case eval world (Repeat (Val 1) cmds) of 
      Left err -> Left err
      Right w' -> Right $ render size (canvas w')


