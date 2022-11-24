import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Variable a = Var a a
  deriving (Show)

semicol :: Parser Char
semicol = char ';'

-- "var a=23;"
variable :: Parser (Variable String)
variable = do
  string "var"
  space
  name <- many1 alphaNum
  char '='
  val <- many1 alphaNum
  semicol
  return $ Var name val

-- "var a=23;"
varapp :: Parser (Variable String)
varapp = Var <$>
         (string "var" *>
          space *>
          many1 alphaNum)
         <* char '='
         <*> many1 alphaNum
         <* semicol

parseKnas :: String -> Either ParseError (Variable String)
parseKnas input = parse varapp "(unknown)" input
