{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (until)

import Control.Applicative
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe

import Interleave

-- Parser and instances {{{

newtype Parser a = Parser
  { runParser :: StateT String Interleave a
  }

instance Functor Parser where
  fmap f (Parser r) = Parser $ fmap f r

instance Monad Parser where
  return  = Parser . return
  (Parser r) >>= f = Parser $
    r >>= runParser . f
  fail str = mzero

instance MonadPlus Parser where
  mzero = Parser mzero
  (Parser r1) `mplus` (Parser r2) =
    Parser (r1 `mplus` r2)

instance Applicative Parser where
  pure = Parser . pure
  (Parser f) <*> (Parser r2) =
    Parser (f <*> r2)

instance Alternative Parser where
  empty = Parser empty
  (Parser r1) <|> (Parser r2) = Parser (r1 <|> r2)

instance MonadState String Parser where
  get = Parser get
  put = Parser . put

-- }}}

-- Operations {{{

-- | run returns a list of potential end states of the parser, each containing a result and final state.
run :: Parser a -> String -> [(a,String)]
run m = runInterleave . runStateT (runParser m)

-- | eval returns a list of potential parses, nubbed by simple equality.
eval :: Eq a => Parser a -> String -> [a]
eval m = loop . run m
  where
  loop = foldr (\(a,_) as-> if a `elem` as then as else a : as) []

-- | push adds another value onto the top of the state.
push :: (MonadPlus m, MonadState [a] m) => a -> m ()
push x = modify (x:)

-- | peek reveals the top value of the state, without consuming it. If the state is empty, it returns Nothing.
peek :: (MonadPlus m, MonadState [a] m) => m (Maybe a)
peek = do
  xs <- get
  case xs of
    []  -> return Nothing
    x:_ -> return $ Just x

-- | pop pulls the top value off the state. It implies a requirement that the state be non-empty.
pop :: (MonadPlus m, MonadState [a] m) => m a
pop = do
  xs <- get
  guard $ not $ null xs
  modify tail
  return $ head xs

-- }}}

-- Combinators {{{

-- | none succeeds only when the given parser fails.
none :: Parser a -> Parser ()
none m = do
  s <- get
  let as = run m s
  if null as
  then return ()
  else mzero

-- | (<+>) returns a tuple of two parsers. It is lifted (,).
(<+>) :: Parser a -> Parser b -> Parser (a,b)
(<+>) = liftA2 (,)

-- | (<:>) is lifted (:).
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

infixr <+>

-- | cat takes two list parsers and concatenates them. It is lifted (++).
cat :: Parser [a] -> Parser [a] -> Parser [a]
cat = liftA2 (++)

-- | poss is a greedy zero-or-one parser.
poss :: Parser a -> Parser (Maybe a)
poss m = do
  r <- (Just <$> m) <|> pure Nothing
  case r of
    Just x -> return (Just x)
    Nothing -> none m >> return Nothing

-- | some' is a greedy one-or-more parser.
some' :: Parser a -> Parser [a]
some' m = some m <* none m

-- | many' is a greedy zero-or-more parser.
many' :: Parser a -> Parser [a]
many' m = many m <* none m

-- | count replicates a parser a given number of times.
count :: Int -> Parser a -> Parser [a]
count = replicateM

-- | assert enforces a predicate over a value in a MonadPlus.
assert :: (MonadPlus m) => (a -> Bool) -> a -> m a
assert p a = guard (p a) >> return a

-- | around wraps a parser in another to the left and right, returning the value of the center parser.
around :: Parser a -> Parser b -> Parser b
w `around` a = w *> a <* w

-- }}}

char :: Char -> Parser Char
char c = chars (== c)

chars :: (Char -> Bool) -> Parser Char
chars p = assert p =<< pop

done :: Parser ()
done = maybe (return ()) (const mzero) =<< peek

string :: String -> Parser String
string s = mapM char s

letter :: Parser Char
letter = chars isAlpha

whiteSpace :: Parser Char
whiteSpace = chars isSpace

word :: Parser String
word = some' letter

digit :: Parser Char
digit = assert isDigit =<< pop

integer :: Parser Integer
integer = read <$> some' digit

double :: Parser Double
double = read <$> do
  n1 <- some' digit
  n2 <- poss (char '.' <:> some' digit)
  return $ maybe n1 (n1++) n2

symbol :: String -> Parser String
symbol s = many' whiteSpace `around` (assert (== s) =<< word)

identifier :: Parser String
identifier = many' whiteSpace `around` word

semi :: Parser Char
semi = many' whiteSpace `around` char ';'

sepBy :: Parser a -> Parser b -> Parser [a]
a `sepBy` s = (a <:> more) <|> pure []
  where
  more = (s *> (a `sepBy` s)) <|> pure []

price :: Parser Int
price = read <$> (some' digit <* char '.') `cat` count 2 digit

receipt :: Parser Bool
receipt = do
  ps <- many' produkt
  p  <- total
  return (sum ps == p)

produkt :: Parser Int
produkt = (do
    symbol "return"
    p <- price
    semi
    return (-p))
  <|> (do
    assert (/= "return") =<< identifier
    p <- price
    semi
    return p)

total :: Parser Int
total = do
  p <- price
  symbol "total"
  return p

