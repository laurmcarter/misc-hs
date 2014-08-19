{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Char

import Text.Show.Pretty
import System.IO

-- IL {{{

newtype IL a = IL
  { runIL :: [a]
  } deriving (Show,Functor)

instance Monad IL where
  return a = IL [a]
  m >>= f  = case runIL m of
    []    -> mzero
    a:as' -> f a `mplus` (IL as' >>= f)

instance MonadPlus IL where
  mzero = IL []
  m1 `mplus` m2 = case runIL m1 of
    []    -> m2
    a:as' -> IL (a : runIL rest)
      where rest = m2 `mplus` IL as'

instance Applicative IL where
  pure  = return
  (<*>) = ap

instance Alternative IL where
  empty = mzero
  (<|>) = mplus

resolve :: IL a -> Maybe a
resolve il = case runIL il of
  [a] -> Just a
  _   -> Nothing

-- }}}

-- Parser {{{

data Parser s a = Parser
  { runParser :: s -> IL (a,s)
  } deriving (Functor)

instance Monad (Parser s) where
  return a = Parser $ \s -> return (a,s)
  p >>= f  = Parser $ \s ->
    let as = runParser p s in
      msum $ runIL $ flip fmap as $ \(a,s') ->
        runParser (f a) s'

instance MonadPlus (Parser s) where
  mzero = Parser $ const mzero
  m1 `mplus` m2 = Parser $ \s ->
    runParser m1 s `mplus` runParser m2 s

instance MonadState s (Parser s) where
  state f = Parser $ return . f

instance Applicative (Parser s) where
  pure  = return
  (<*>) = ap

instance Alternative (Parser s) where
  empty = mzero
  (<|>) = mplus

type Parse = Parser PState

-- }}}

-- Parsed {{{

data Parsed a = Parsed
  { parsed :: a
  , parsePos :: Pos
  } deriving (Eq,Show)

type PChar = Parsed Char
type PString = Parsed String

-- }}}

-- Pos {{{

type Pos = (Int,Int)

_x :: (Field1 s t a b) => Lens s t a b
_x = _1
_y :: (Field2 s t a b) => Lens s t a b
_y = _2

-- }}}

-- PState {{{

data PState = PState
  { _curPos :: Pos
  , _input  :: String
  } deriving (Eq,Show)

initState :: String -> PState
initState = PState (0,0)

makeLenses ''PState

-- }}}

data Tile = Tile
  { _pos  :: Pos
  , _here :: Maybe Thing
  } deriving (Eq)

instance Show Tile where
  show (Tile p h) = "{" ++ show h ++ " " ++ show p ++ "}"

data Thing
  = Rock
  | Grass
  | Human
  deriving (Eq)

instance Show Thing where
  show = \case
    Rock  -> "o"
    Grass -> "\""
    Human -> "@"

makeLenses ''Tile

getPos :: a -> Parse (Parsed a)
getPos a = Parsed a <$> use curPos

next :: Parse PChar
next = do
  mt <- uses input null
  if mt
  then mzero
  else do
    c <- use $ input . to head
    input %= tail
    if c == '\n'
    then do
      curPos._x .= 0
      curPos._y += 1
    else do
      curPos._x += 1
    getPos c

contiguous :: [Parsed Char] -> Parse PString
contiguous ps = if null ps
  then getPos ""
  else return $ Parsed (map parsed ps) $ head $ map parsePos ps

char :: Char -> Parse PChar
char c = do
  p <- next
  guard (parsed p == c)
  return p

string :: String -> Parse PString
string s = do
  cs <- mapM char s
  contiguous cs

nextChar :: Parse PChar
nextChar = do
  p <- next
  if parsed p == '\n'
  then nextChar
  else return p

joinA :: Alternative f => [f a] -> f a
joinA as = case as of
  []    -> empty
  a:as' -> a <|> joinA as'

oneOf :: [Char] -> Parse PChar
oneOf = joinA . map char

whitespace :: Parse PString
whitespace = do
  st <- use curPos
  ps <- many $ oneOf " \t\n\r"
  contiguous ps

tile :: Parse (Maybe Tile)
tile = do
  p <- nextChar
  let mkTile = return . Just . Tile (parsePos p)
  case parsed p of
    '.' -> mkTile Nothing
    '@' -> mkTile $ Just Human
    '"' -> mkTile $ Just Grass
    'o' -> mkTile $ Just Rock
    ' ' -> noTile
    _   -> mzero
  where
  noTile :: Parse (Maybe Tile)
  noTile = return Nothing

greedy :: (MonadPlus f) => f a -> f [a]
greedy t = t <|> 

renderTile :: Maybe Tile -> String
renderTile = \case
  Just t  -> show t
  Nothing -> " "
