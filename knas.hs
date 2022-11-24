import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

data Variable a = Var a a
  deriving (Show)

semicol :: Parser Char
semicol = char ';'

variable :: Parser (Variable String)
variable = do
  string "var"
  space
  name <- many1 alphaNum
  char '='
  val <- alphaNum
  semicol
  return $ Var name [val]

parseKnas :: String -> Either ParseError (Variable String)
parseKnas input = parse variable "(unknown)" input
