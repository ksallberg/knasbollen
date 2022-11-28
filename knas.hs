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

semicol :: Parser Char
semicol = char ';'

-- "varde a=23;"
variable :: Parser Stmt
variable = Var <$>
           (spaces *>
            string "varde" *>
            spaces *>
            many1 alphaNum <* spaces)
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
conditional = do
  spaces
  cond <- condition
  spaces
  next <- block
  spaces
  return $ If cond next

block :: Parser Stmt
block = do
  char '{'
  spaces
  x <- many (variable <|> conditional)
  spaces
  char '}'
  return $ Block x

parseKnas :: String -> Either ParseError Stmt
parseKnas input = parse conditional "(unknown)" input

main :: IO ()
main = do
  -- args <- getArgs
  contents <- readFile "hello.knas" --(head args)
  let x = parse block "unknown" contents
  putStrLn (show x)
  return ()

testProgInput :: String
testProgInput = unlines
  ["{",
   "  varde apa=2;",
   "varde bepa=3; ",
   "}"]

testProg :: Either ParseError Stmt
testProg = parse block "(unknown)" testProgInput
