{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Chess where

import Data.Char
import Data.List

-- Types {{{

data Board' a = Board' (Row' a) (Row' a) (Row' a) (Row' a) (Row' a) (Row' a) (Row' a) (Row' a) deriving (Show,Functor)
data Row' a = Row' (Square' a) (Square' a) (Square' a) (Square' a) (Square' a) (Square' a) (Square' a) (Square' a) deriving (Show,Functor)
newtype Square' a = Square (Maybe a) deriving (Eq,Show,Functor)

type Board = Board' Piece
type Row = Row' Piece
type Square = Square' Piece

data Piece = Piece PColor PType deriving (Eq,Show)
data PType
  = Pawn
  | Rook
  | Knight
  | Bishop
  | Queen
  | King
  deriving (Eq,Show)
data PColor = Black | White deriving (Eq,Show)

type Loc = (ColLoc,RowLoc)
data ColLoc
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Eq,Ord,Show,Enum)
data RowLoc
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  deriving (Eq,Ord,Show,Enum)

col :: ColLoc -> Row -> Square
col cl (Row' a b c d e f g h) = case cl of
  A -> a
  B -> b
  C -> c
  D -> d
  E -> e
  F -> f
  G -> g

row :: RowLoc -> Board -> Row
row rl (Board' _1 _2 _3 _4 _5 _6 _7 _8) = case rl of
  One   -> _1
  Two   -> _2
  Three -> _3
  Four  -> _4
  Five  -> _5
  Six   -> _6
  Seven -> _7
  Eight -> _8

square :: Loc -> Board -> Square
square (c,r) = col c . row r

-- }}}

-- Pretty {{{

class Pretty a where
  pretty :: a -> String

instance Pretty Piece where
  pretty (Piece c t) = case c of
    Black -> [toLower t']
    White -> [t']
    where t' = head $ show t

instance Pretty Square where
  pretty (Square mp) = case mp of
    Just p  -> pretty p
    Nothing -> " "

instance Pretty Row where
  pretty (Row' _1 _2 _3 _4 _5 _6 _7 _8) =
    let ls  = map pretty [_1,_2,_3,_4,_5,_6,_7,_8] in
      wrap "|" $ intercalate "|" ls

instance Pretty Board where
  pretty (Board' a b c d e f g h) =
    let ls = map pretty [a,b,c,d,e,f,g,h]
        nonPieceGen = [ '-', '+' ] ++ nonPieceGen
        nonPieceLine = wrap "|" $ take 15 nonPieceGen
        borderLine = wrap "+" $ replicate 15 '-' in
      intercalate "\n" $ wrap [borderLine] $ intersperse nonPieceLine ls

wrap :: [a] -> [a] -> [a]
wrap s l = s ++ l ++ s

-- }}}

-- Constructors {{{

emptySquare = Square Nothing

emptyRow = Row' s s s s s s s s
  where s = emptySquare

emptyBoard :: Board
emptyBoard = Board' r r r r r r r r
  where r = emptyRow

white :: Piece -> Piece
white (Piece _ t) = Piece White t

black :: Piece -> Piece
black (Piece _ t) = Piece Black t

piece :: Piece -> Square
piece = Square . Just

homeRow c = Row'
  (p Rook)
  (p Knight)
  (p Bishop)
  (p Queen)
  (p King)
  (p Bishop)
  (p Knight)
  (p Rook)
  where p = place c

place :: PColor -> PType -> Square
place c = Square . Just . Piece c

pawnRow c = Row' p p p p p p p p
  where p = place c Pawn

startBoard :: Board
startBoard = Board'
  (homeRow White)
  (pawnRow White)
  emptyRow
  emptyRow
  emptyRow
  emptyRow
  (pawnRow Black)
  (homeRow Black)

-- }}}

pieceOn :: Loc -> Chess (Maybe Piece)
pieceOn l = do
  Square mp <- gets $ square l
  return mp

forward :: PColor -> RowLoc -> RowLoc

newtype Chess a = Chess { runChess :: Board -> (a,Board) } deriving (Functor)

instance Monad Chess where
  return a = Chess $ \b -> (a,b)
  m >>= f  = Chess $ \b -> let (a,b') = runChess m b in runChess (f a) b'



