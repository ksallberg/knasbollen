import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Condition a = Gt a a |
                   Lt a a |
                   Gte a a |
                   Lte a a |
                   Eq a a
  deriving (Show)

data Stmt = Block [Stmt] |
            If (Condition String) Stmt |
            IfThenElse (Condition String) Stmt Stmt |
            Var String String
  deriving (Show)

data Prog = Program [Stmt]
  deriving (Show)

semicol :: Parser Char
semicol = char ';'

-- "varde a=23;"
variable :: Parser Stmt
variable =
  Var <$>
  (spaces *> string "varde" *> spaces *> many1 alphaNum <* spaces)
  <* char '='
  <* spaces
  <*> many1 alphaNum
  <* spaces
  <* semicol
  <* spaces

selectCond :: String -> String -> String -> Condition String
selectCond "==" a b = Eq a b
selectCond "<" a b = Lt a b
selectCond ">" a b = Gt a b

-- "utifallAtt(x==23)"
condition :: Parser (Condition String)
condition = do
  string "utifallAtt"
  spaces
  char '('
  spaces
  var <- many1 alphaNum
  spaces
  comp <- choice [string "==", string "<", string ">"]
  spaces
  val <- many1 alphaNum
  spaces
  char ')'
  return (selectCond comp var val)

conditional :: Parser Stmt
conditional = If <$>
              (spaces *> condition <* spaces)
              <*> block
              <* spaces

block :: Parser Stmt
block = do
  char '{'
  spaces
  x <- many (variable <|> conditional)
  spaces
  char '}'
  return $ Block x

program :: Parser Prog
program = Program <$>
          (spaces *>
           many (block <|>
                 conditional <|>
                 variable))
          <* spaces

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->
      putStrLn "Fel! Du måste ge en knasig fil"
    (file : _) -> do
      contents <- readFile file
      let x = parse program "unknown" contents
      putStrLn (show x)

test :: IO ()
test = do
  contents <- readFile "hello.knas"
  let x = parse program "unknown" contents
  putStrLn (show x)
  return ()
