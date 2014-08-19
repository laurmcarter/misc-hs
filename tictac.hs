{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.List

data Piece = X | O deriving (Eq,Show)

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

--data GameStatus (turn :: Piece)
--  = Turn (ProcessMove turn)
--  | Draw
--  | Win Piece

data GameStatus (turn :: Piece) where
  Turn :: ProcessMove turn -> GameStatus turn
  Draw :: GameStatus turn
  Win  :: Piece -> GameStatus turn

type Board = [[Maybe Piece]]
type Position = (Int,Int)
type ProcessMove turn = Move turn -> Maybe (Game (Other turn))
data Move (turn :: Piece) = Move Position deriving (Eq,Show)

newBoard :: Board
newBoard = replicate 3 $ replicate 3 Nothing

putPiece :: Piece -> Position -> Board -> Board
putPiece p (r,c) b = imap (\r' -> imap (\c' mp -> if r == r' && c == c' then Just p else mp)) b

isFull   :: Board -> Bool
isFull = all (all isJust)

(!)      :: Board -> Position -> Maybe Piece
b ! p@(r,c)
  | r >= 0 && r < 3 &&
    c >= 0 && c < 3
    = ((b !! r) !! c)
  | otherwise = error ("bad position: " ++ show p)

lanes    :: [[Position]]
lanes =
  [ [ (r,c) | c <- [0..2] ] | r <- [0..2] ] ++
  [ [ (r,c) | r <- [0..2] ] | c <- [0..2] ] ++
  [ [ (r,r) | r <- [0..2] ] , [ (r,2-r) | r <- [0..2] ] ]

movePos  :: Move turn -> Position
movePos (Move p) = p

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
  nextTurn = Just $ Turn $ makeMove board'
  draw     = maybeIf (isFull board') Draw
  victory  = msum $ map check lanes
  check l  = maybeIf allMatch $ Win piece
    where
    allMatch = all (\p -> board' ! p == Just piece) l

type CyclicTurn turn =
  ( ReifyPiece turn
  , ReifyPiece (Other turn)
  , Other (Other turn) ~ turn
  )

maybeIf :: Bool -> a -> Maybe a
maybeIf p a = guard p >> return a

renderBoard :: Board -> IO ()
renderBoard b = do
  renderRow (b !! 0)
  renderBorder
  renderRow (b !! 1)
  renderBorder
  renderRow (b !! 2)
  where
  renderRow = putStrLn . intercalate "|" . map renderPiece
  renderBorder = putStrLn "-+-+-"
  renderPiece :: Maybe Piece -> String
  renderPiece = maybe " " show

playGame :: IO ()
playGame = loop newGame
  where
  loop :: CyclicTurn turn => Game turn -> IO ()
  loop gm@(Game b st) = do
    renderBoard b
    case st of
      Turn pt -> do
        putStrLn "what's your move?"
        m <- readLn
        case pt (Move m) of
          Just g -> loop g
          Nothing -> do
            putStrLn "you dun goofed"
            loop gm
      Draw   -> putStrLn "the game *draws* to a close! Eh? Eh?"
      Win p  -> putStrLn (show p ++ " wins the game!")
    

