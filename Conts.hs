{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ContsT where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State

import Data.Char
import qualified Data.Map as M
import Data.Monoid

-- ContsT {{{

newtype ContsT fl r m a = ContsT
  { runContsT :: (fl -> m r) -> (a -> m r) -> m r
  }

instance Functor (ContsT fl r m) where
  fmap f m = ContsT $ \fl sc -> runContsT m fl (sc . f)

instance Monad (ContsT fl r m) where
  return a = ContsT $ \_  sc -> sc a
  m >>= f  = ContsT $ \fl sc ->
    runContsT m fl $ \a ->
      runContsT (f a) fl sc

failContsT :: fl -> ContsT fl r m a
failContsT e = ContsT $ \fl _ -> fl e

instance Monoid fl => MonadPlus (ContsT fl r m) where
  mzero = failContsT mempty
  mplus m1 m2 = ContsT $ \fl sc ->
    flip (runContsT m1) sc $ \e ->
      flip (runContsT m2) sc $ \e' ->
        fl (e <> e')

instance Applicative (ContsT fl r m) where
  pure  = return
  (<*>) = ap

instance Monoid fl => Alternative (ContsT fl r m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans (ContsT fl r) where
  lift m = ContsT $ \fl sc -> m >>= sc

instance (MonadState s m) => MonadState s (ContsT fl r m) where
  get = lift get
  put = lift . put

-- }}}

-- Parser {{{

data PState u p a = PS
  { _input :: [a]
  , _pos   :: p
  , _user  :: u
  } deriving (Eq,Show)

makeLenses ''PState

type Parser r = ContsT String r (State (PState Int Int Char))

pop :: Parser r Char
pop = do
  i <- lift $ use input
  if null i
  then mzero
  else do
    lift $ do
      input %= tail
      pos   += 1
    return $ head i

peek :: Parser r (Maybe Char)
peek = do
  s <- lift get
  return $
    if null $ _input s
    then Nothing
    else Just $ head $ s ^. input

push :: Char -> Parser r ()
push c = do
  input %= (c:)
  p <- use pos
  unless (p == 0) $
    pos -= 1

char :: Char -> Parser r Char
char c = chars (c ==)

chars :: (Char -> Bool) -> Parser r Char
chars pr = assert pr =<< pop

done :: Parser r ()
done = maybe (return ()) (const mzero) =<< peek

string :: String -> Parser r String
string s = mapM char s

letter :: Parser r Char
letter = chars isAlpha

whiteSpace :: Parser r Char
whiteSpace = chars isSpace

word :: Parser r String
word = some' letter

digit :: Parser r Char
digit = assert isDigit =<< pop

integer :: Parser r Integer
integer = read <$> some' digit

double :: Parser r Double
double = read <$> do
  n1 <- some' digit
  n2 <- poss (char '.' <:> some' digit)
  return $ maybe n1 (n1++) n2

symbol :: String -> Parser r String
symbol s = many' whiteSpace `around` (assert (== s) =<< word)

identifier :: Parser r String
identifier = many' whiteSpace `around` word

semi :: Parser r Char
semi = many' whiteSpace `around` char ';'

sepBy :: Parser r a -> Parser r b -> Parser r [a]
a `sepBy` s = (a <:> more) <|> pure []
  where
  more = (s *> (a `sepBy` s)) <|> pure []

-- }}}

-- Combinators {{{

try :: Parser r a -> Parser r a
try m = ContsT $ \fl sc ->
  runContsT m sc sc

none :: Parser r a -> Parser r ()
none m = undefined --ContsT $ \fl sc ->
  --runContsT m
  --  (\e -> 
  --  (\a -> mzero)

(<+>) :: Parser r a -> Parser r b -> Parser r (a,b)
(<+>) = liftA2 (,)
infixr <+>

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)
infixr <:>

cat :: Parser r [a] -> Parser r [a] -> Parser r [a]
cat = liftA2 (++)

poss :: Parser r a -> Parser r (Maybe a)
poss m = do
  r <- (Just <$> m) <|> pure Nothing
  case r of
    Just x -> return (Just x)
    Nothing -> none m >> return Nothing

some' :: Parser r a -> Parser r [a]
some' m = some m <* none m

many' :: Parser r a -> Parser r [a]
many' m = many m <* none m

count :: Int -> Parser r a -> Parser r [a]
count = replicateM

assert :: (MonadPlus m) => (a -> Bool) -> a -> m a
assert p a = guard (p a) >> return a

around :: Parser r a -> Parser r b -> Parser r b
w `around` a = w *> a <* w

-- }}}

-- JSON {{{

data JSON
  = JMap (M.Map String JSON)
  | JArray [JSON]
  | JString String
  | JInt Int
  | JBool Bool
  | JNull
  deriving (Eq,Show)

type JParser = Parser JSON



-- }}}

