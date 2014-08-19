{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Applicative

data Piece = X | O deriving (Eq)

class ReifyPiece (p :: Piece) where
  reifyPiece :: f p -> Piece

instance ReifyPiece X where
  reifyPiece _ = X

instance ReifyPiece O where
  reifyPiece _ = O

type family Other (p :: Piece) :: Piece
type instance Other X = O
type instance Other O = X

data Game (turn :: Piece) = Game Board (GameStatus turn)
type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

data GameStatus (turn :: Piece) where
  Turn :: ProcessMove turn -> GameStatus turn
  Draw :: GameStatus turn
  Win  :: Piece -> GameStatus turn

newBoard :: Board
putPiece :: Piece -> Position -> Board -> Board
isFull   :: Board -> Bool
(!)      :: Board -> Position -> Maybe Piece

lanes    :: [[Position]]
movePos  :: Move turn -> Position

newGame  :: Game X
newGame   = Game b $ Turn $ makeMove b
  where b = newBoard

makeMove :: CyclicTurn turn => Board -> ProcessMove turn
makeMove board move
  | Nothing   <- board ! pos = Game board' <$> (gameOver <|> nextTurn)
  | otherwise                = Nothing
  where
  piece    = reifyPiece move
  pos      = movePos move
  board'   = putPiece piece pos board
  gameOver = victory <|> draw
  draw     = maybeIf (isFull board') Draw
  victory  = msum $ map check lanes
  check l  = maybeIf allMatch $ Win piece
  allMatch = all (\p -> board' ! p == Just piece) lane

type CyclicTurn turn =
  ( ReifyPiece turn
  , ReifyPiece (Other turn)
  , Other (Other turn) ~ turn
  )

maybeIf :: Bool -> a -> Maybe a
maybeIf p a = guard p >> return a

