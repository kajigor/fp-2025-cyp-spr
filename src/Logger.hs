module Logger where

data Logger log value = Logger [log] value deriving (Show, Eq)

-- TODO: Implement the Functor instance
instance Functor (Logger log) where
    fmap :: (a -> b) -> Logger log a -> Logger log b
    fmap f (Logger logs value) = Logger logs (f value)

{-
fmap id x == x
fmap id (Logger logs value) = Logger logs (id value) = Logger logs value

fmap (f . g) x == fmap f (fmap g x)
fmap (f . g) x =
fmap (f . g) (Logger logs value) = Logger logs ((f . g) value) = Logger logs (f (g value)) =
= fmap f (Logger logs (g value)) = fmap f (fmap g (Logger logs value)) =
= fmap f (fmap g x)
-}

-- TODO: Implement the Applicative instance
instance Applicative (Logger log) where
    pure :: a -> Logger log a
    pure value = Logger [] value

    (<*>) :: Logger log (a -> b) -> Logger log a -> Logger log b
    (<*>) (Logger logs1 fun) (Logger logs2 value) = Logger (logs1 ++ logs2) (fun value)

{-

pure id <*> v = v
pure id <*> Logger logs value =
= Logger [] id <*> Logger logs value =
= Logger ([] ++ logs) (id value) =
= Logger logs value

pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
pure (.) <*> Logger logs1 f <*> Logger logs2 g <*> Logger l3 x =
= Logger [] (.) <*> Logger logs1 f =
= Logger logs1 (.(f)) <*> Logger logs2 g =
= Logger (logs1 ++ logs2) (f . g) <*> Logger l3 x =
= Logger (logs1 ++ logs2 ++ l3) ((f . g) x)
= Logger (logs1 ++ logs2 ++ l3) (f (g x))

At the same time
Logger logs1 f <*> (Logger logs2 g <*> Logger l3 x)
= Logger logs2 g <*> Logger l3 x = Logger (logs2 ++ l3) (g x)
= Logger logs1 f <*> Logger (logs2 ++ l3) (g x)
= Logger (logs1 ++ logs2 ++ l3) (f (g x))


pure f <*> pure x = pure (f x)
pure f <*> pure x
= Logger [] f <*> Logger [] x
= Logger ([] ++ []) (f x)
= Logger [] (f x)
= pure (f x)

Logger logs f <*> pure y
= Logger logs f <*> Logger [] y
= Logger (logs ++ []) (f y)

u <*> pure y = pure ($ y) <*> u
pure ($ y) <*> Logger logs f
= Logger [] ($ y) <*> Logger logs f
= Logger ([] ++ logs) (($ y) f)
= Logger logs (f y)
-}

-- TODO: Implement the Monad instance
instance Monad (Logger log) where
    return :: a -> Logger log a
    return = pure

    (>>=) :: Logger log a -> (a -> Logger log b) -> Logger log b
    Logger logs1 valueA >>= funValue2Loger =
      let (Logger logs2 valueB) = (funValue2Logger valueA) in
      Logger (logs1 ++ logs2) valueB
      
{-
return a >>= f
= Logger [] a >>= f
= let Logger logs2 valueB = f a
  in Logger ([] ++ logs2) valueB
= f a

Logger logs value >>= return
= let Logger logs2 valueB = return value = Logger [] value
  in Logger (logs ++ []) value
= Logger logs value

(m >>= f) >>= g == m >>= (\x -> f x >>= g)
m = Logger logs1 x
f x = Logger logs2 y
g y = Logger l3 z

On the LHS:
(Logger logs1 x >>= f) >>= g
= Logger (logs1 ++ logs2) y >>= g
= let Logger l3 z = g y
  in Logger ((logs1 ++ logs2) ++ l3) z

On the RHS:
Logger logs1 x >>= (\x -> f x >>= g)
= let Logger logs2 y = f x
  in y >>= g = Logger logs2 y >>= g = Logger (logs2 ++ l3) z
=> Logger logs1 x >>= (\x -> Logger (logs2 ++ l3) z)
= Logger (logs1 ++ (logs2 ++ l3)) z
-}

-- Writes a single log message. 
-- Can be easily bound together with other logging computations.
writeLog :: log -> Logger log ()
writeLog log = Logger [log] () 

-- Logs every intermediate result 
-- ghci> factLog 5
-- Logger [(0,1),(1,1),(2,2),(3,6),(4,24),(5,120)] 120
factLog :: Int -> Logger (Int, Int) Int 
factLog n 
  | n <= 0 = do 
      let res = 1 
      writeLog (n, res)
      return res 
  | otherwise = do 
      prev <- factLog (n - 1)
      let res = n * prev 
      writeLog (n, res)
      return res 

          