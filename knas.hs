import Control.Monad
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Prog = Program [Stmt]
  deriving (Show)

data Stmt = Block [Stmt] |
            If (Condition String) Stmt |
            IfThenElse (Condition String) Stmt Stmt |
            Var String String |
            Assign String String |
            Loop (Condition String) Stmt |
            Print String
  deriving (Show)

data Condition a = Gt a a |
                   Lt a a |
                   Gte a a |
                   Lte a a |
                   Eq a a
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
condition :: String -> Parser (Condition String)
condition word =
  selectCond <$>
  (string word *>
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
               (condition "utifallAtt")
               <* spaces)
              <*> block
              <* spaces

-- "utifallAtt(x==23) { ... } annarsDårå { ... }"
conditionalElse :: Parser Stmt
conditionalElse = IfThenElse <$>
  (spaces *> (condition "utifallAtt") <* spaces)
  <*>
  block
  <*>
  (spaces *> string "annarsDårå" *> spaces *> block <* spaces)

loop :: Parser Stmt
loop = Loop <$>
  (spaces *> (condition "springOmkringTills") <* spaces)
  <*> block

pprint :: Parser Stmt
pprint = Print <$>
         (spaces *>
          string "skrik" *>
          char '(' *>
          char '"' *>
          many1 (choice [alphaNum, space]))
         <* char '"'
         <* char ')'
         <* char ';'

assign :: Parser Stmt
assign = Assign <$>
         (spaces *>
          many1 alphaNum
          <* spaces
          <* char '='
          <* spaces)
         <*>
         (many1 alphaNum)
         <* char ';'

substmts :: Parser [Stmt]
substmts = many $ try variable <|>
                  try conditionalElse <|>
                  try conditional <|>
                  try loop <|>
                  try pprint <|>
                  try assign

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
