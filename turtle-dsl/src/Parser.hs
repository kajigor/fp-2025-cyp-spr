module Parser (
  Command(..),
  Expr(..),
  parseProgram
) where


import Data.Void  (Void)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Expr = Val Int | Var String
  deriving (Show)

data Command = 
    Forward Expr
  | TurnLeft 
  | TurnRight
  | PenUp
  | PenDown
  | Repeat  Expr   [Command]
  | Let     String Expr
  deriving (Show)

-- space consumer, consumes all spaces, newlines, tabs etc.
sc :: Parser ()
sc = L.space space1 empty empty 

-- helper that just parses a thing and skips all spaces, newlines etc.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- parse exact string
symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Int
integer = lexeme L.decimal

-- parses identifiers such as 'x', 'meow42', but not '1meow', '245'
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)


expr :: Parser Expr
expr = (Val <$> integer) <|> (Var <$> identifier)


-- Parses 10x10 or smth to have size of field
fieldSize :: Parser (Maybe (Int, Int))
fieldSize = optional $ try $ do
  w <- L.decimal
  _ <- char 'x'
  h <- L.decimal
  _ <- many (char ' ' <|> char '\t') <* eol
  return (w, h)

letCmd :: Parser Command
letCmd = do
  _    <- symbol "let" 
  name <- identifier
  _    <- symbol "="
  Let name <$> expr
 
forwardCmd :: Parser Command
forwardCmd = Forward <$> (symbol "forward" *> expr)

leftCmd :: Parser Command
leftCmd = TurnLeft <$ symbol "left"

rightCmd :: Parser Command
rightCmd = TurnRight <$ symbol "right"

penUpCmd :: Parser Command
penUpCmd = PenUp <$ symbol "penup"

penDownCmd :: Parser Command
penDownCmd = PenDown <$ symbol "pendown"

-- parse commands like "repeat x [ forward x ]"
repeatCmd :: Parser Command
repeatCmd = do
  _    <- symbol "repeat"
  n    <- expr
  cmds <- between (symbol "[") (symbol "]") (many command)
  pure (Repeat n cmds)

command :: Parser Command
command = choice . map try $ [
    letCmd
  , forwardCmd
  , rightCmd
  , leftCmd
  , repeatCmd
  , penUpCmd
  , penDownCmd
  ]


programParser :: Parser ((Int, Int), [Command])
programParser = do
  sc
  msize <- fieldSize
  cmds  <- many command
  eof
  let fsize = fromMaybe (10, 10) msize -- by deault i take field size (10, 10)
  return (fsize, cmds)

parseProgram :: String -> Either String ((Int, Int), [Command])
parseProgram txt =
  case runParser programParser "input" txt of
    Left err -> Left (errorBundlePretty err)
    Right prog -> Right prog

