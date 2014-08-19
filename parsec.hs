
import Data.Char

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr

import qualified Text.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser $
  haskellDef
    { P.reservedOpNames = ["*","/","+","-"]
    , P.reservedNames   = ["return","total"]
    }

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer

expr :: Parser Integer
expr = buildExpressionParser table factor <?> "expression"

table = [ [ op "*" (*) AssocLeft
          , op "/" div AssocLeft
          ]
        , [ op "+" (+) AssocLeft
          , op "-" (-) AssocLeft
          ]
        ]
  where
  op s f assoc = Infix
    ((reservedOp s >> return f) <?> "operator")
    assoc

factor = parens expr
  <|> natural
  <?> "simple expression"

runLex :: Show a => Parser a -> String -> IO ()
runLex p = parseTest $ do
  whiteSpace
  x <- p
  eof
  return x

price :: Parser Int
price = lexeme prs <?> "price"
  where
  prs = do
    ds1 <- many1 digit
    char '.'
    ds2 <- count 2 digit
    return (convert 0 (ds1 ++ ds2))
  convert = foldl $ \n d -> 10 * n + digitToInt d

receipt :: Parser Bool
receipt = do
  ps <- many produkt
  p <- total
  return (sum ps == p)

produkt :: Parser Int
produkt = do
    reserved "return"
    p <- price
    semi
    return (-p)
  <|> do
    identifier
    p <- price
    semi
    return p
  <?> "produkt"

total :: Parser Int
total = do
  p <- price
  reserved "total"
  return p

