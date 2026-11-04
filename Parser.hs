{-# LANGUAGE LambdaCase #-}

-- export списъкът да НЕ се променя
module Parser
  ( Parser,
    item,
    sat,
    but,
    peek,
    eof,
    many,
    many1,
    optional,
    between,
    sepBy,
    sepBy1,
    choice,
    oneOf,
    notOneOf,
    range,
    count,
    label,
    lookahead,
    chainl1,
    Parseable (..),
    ParseError (..),
  )
where

import Control.Applicative qualified as App

data ParseError
  = UnknownError
  | InputNotExhausted
  | EmptyStream
  | UnexpectedSymbol String
  deriving (Show)

newtype Parser a = MkParser {runParser :: String -> Either ParseError (a, String)}

instance Functor Parser where
  fmap f (MkParser p) =
    MkParser $ \s -> do (x, s') <- p s; pure (f x, s')

instance Applicative Parser where
  pure x = MkParser $ \s -> Right (x, s)

  MkParser p1 <*> MkParser p2 =
    MkParser $ \s -> do
      (f, s') <- p1 s
      (x, s'') <- p2 s'
      pure (f x, s'')

instance Monad Parser where
  return = pure

  MkParser p >>= f =
    MkParser $ \s -> do
      (x, s') <- p s
      let MkParser pf = f x
      pf s'

instance App.Alternative Parser where
  empty = MkParser . const . Left $ UnknownError

  MkParser p1 <|> MkParser p2 =
    MkParser $ \s ->
      case (p1 s, p2 s) of
        (Right lhs, _) -> Right lhs
        (_, rhs) -> rhs

item :: Parser Char
item = MkParser $ \case
  [] -> Left EmptyStream
  h : t -> pure (h, t)

sat :: (Char -> Bool) -> Parser Char
sat p = MkParser $ \case
  h : t
    | p h -> pure (h, t)
    | otherwise -> Left $ UnexpectedSymbol [h]
  _ -> Left EmptyStream

but :: (Char -> Bool) -> Parser Char
but p = sat (not . p)

peek :: Parser Char
peek = MkParser $ \case
  s@(h : _) -> pure (h, s)
  _ -> Left EmptyStream

eof :: Parser ()
eof = MkParser $ \case
  [] -> pure ((), [])
  _ -> Left InputNotExhausted

between :: Parser s -> Parser a -> Parser s -> Parser a
between l p r = l *> p <* r

sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy delim p = sepBy1 delim p App.<|> pure []

sepBy1 :: Parser sep -> Parser a -> Parser [a]
sepBy1 delim p = liftA2 (:) p $ many $ delim >> p

many :: Parser a -> Parser [a]
many p = many1 p App.<|> pure []

many1 :: Parser a -> Parser [a]
many1 p = liftA2 (:) p $ many p

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p App.<|> pure Nothing

count :: Int -> Parser a -> Parser [a]
count 0 _ = pure []
count n p
  | n < 0 = App.empty
  | otherwise = liftA2 (:) p $ count (n - 1) p

range :: Int -> Int -> Parser a -> Parser [a]
range m n p
  | m < 0 || n < 0 = App.empty
  | otherwise = foldr ((App.<|>) . flip count p) (pure []) [n, n - 1 .. m]

choice :: [Parser a] -> Parser a
choice = App.asum

oneOf :: [Char] -> Parser Char
oneOf cs = sat (`elem` cs)

notOneOf :: [Char] -> Parser Char
notOneOf cs = sat (`notElem` cs)

label :: ParseError -> Parser a -> Parser a
label err (MkParser p) = MkParser $ \s ->
  case p s of
    Left _ -> Left err
    Right result -> Right result

lookahead :: Parser a -> Parser a
lookahead (MkParser p) = MkParser $ \s ->
  case p s of
    Left err -> Left err
    Right (val, _) -> Right (val, s)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 atomp opp = atomp >>= chain
  where
    lr lhs = opp >>= \op -> op lhs <$> atomp
    chain acc = (lr acc >>= chain) App.<|> pure acc

class Parseable a where
  parser :: Parser a

  run :: String -> Either ParseError a
  run s = fst <$> runParser parser s
