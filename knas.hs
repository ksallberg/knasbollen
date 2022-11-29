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
variable = Var <$>
  (spaces *>
   string "varde" *>
   spaces *>
   many1 alphaNum
   <* spaces
   <* char '='
   <* spaces)
  <*> many1 alphaNum
  <* spaces
  <* semicol
  <* spaces

selectCond :: String -> String -> String -> Condition String
selectCond a "==" b = Eq a b
selectCond a "<" b = Lt a b
selectCond a ">" b = Gt a b

-- "utifallAtt(x==23)"
condition :: Parser (Condition String)
condition = selectCond <$>
            (string "utifallAtt" *>
             spaces *>
             char '(' *>
             spaces *>
             (many1 alphaNum))
            <* spaces
            <*> (choice [string "==", string "<", string ">"])
            <* spaces
            <*> many1 alphaNum
            <* spaces
            <* char ')'

-- "utifallAtt(x==23) { ... }"
conditional :: Parser Stmt
conditional = If <$>
              (spaces *>
               condition
               <* spaces)
              <*> block
              <* spaces

-- "utifallAtt(x==23) { ... } annarsDårå { ... }"
conditionalElse :: Parser Stmt
conditionalElse = IfThenElse <$>
  (spaces *> condition <* spaces)
  <*>
  block
  <*>
  (spaces *> string "annarsDårå" *> spaces *> block <* spaces)

substmts :: Parser [Stmt]
substmts = (many $ variable <|>
                   try conditionalElse <|>
                   conditional)

block :: Parser Stmt
block = Block <$> (char '{' *> spaces *>) substmts <* spaces <* char '}'

program :: Parser Prog
program = Program <$> (spaces *> substmts) <* spaces

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
