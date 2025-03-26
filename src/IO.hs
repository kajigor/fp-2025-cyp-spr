module IO where 

-- TODO: Implement reading line from standard input. 
-- Use getChar to read a single character. 
myGetLine :: IO String 
myGetLine = do
    char <- getChar
    if char == '\n' then return []
    else do
     tail <- myGetLine
     return (char : tail)

-- TODO: Ask the user for their name.
-- Print "Hello, NAME" to the standard output, where NAME is the name of the user.
-- Use myGetLine.
helloUser :: IO () 
helloUser = do
  putStrLn "What is your name?"
  name <- myGetLine
  putStrLn ("Hello, " ++ name)

-- TODO: Use interact in helloUser.
helloUser' :: IO () 
helloUser' = interact (\name -> "What is your name?\n" ++ "Hello, " ++ name ++ "\n")


main :: IO ()
main = helloUser'

