module Example where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (isDigit)
import Parser

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show)

char :: Char -> Parser Char
char c = sat (== c)

ws :: Parser ()
ws = void $ many $ oneOf " \t\n"

token :: Parser a -> Parser a
token p = ws *> p <* ws

digit :: Parser Char
digit = sat isDigit

number :: Parser Int
number = read <$> liftA2 (++) handleNeg (many1 digit)
  where
    handleNeg = maybe "" (: []) <$> optional (char '-')

atom :: Parser Expr
atom = token $ Lit <$> number

addNaive :: Parser Expr
addNaive = do
  lhs <- exprNaive
  _ <- char '+'
  rhs <- exprNaive
  pure $ Add lhs rhs

exprNaive :: Parser Expr
exprNaive = addNaive <|> atom

add :: Parser (Expr -> Expr -> Expr)
add = char '+' >> pure Add

sub :: Parser (Expr -> Expr -> Expr)
sub = char '-' >> pure Sub

expr :: Parser Expr
expr = chainl1 atom $ choice [add, sub]

instance Parseable Expr where
  parser = expr <* eof

-- >>> (run "1 +2-3") :: Either ParseError Expr
-- Right (Sub (Add (Lit 1) (Lit 2)) (Lit 3))

-- >>> (run "1 + 2   -") :: Either ParseError Expr
-- Left InputNotExhausted

-- >>> (run "+") :: Either ParseError Expr
-- Left (UnexpectedSymbol "+")

-- >>> (run "-3   ") :: Either ParseError Expr
-- Right (Lit (-3))
