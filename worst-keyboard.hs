{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Reactive.Banana
import Reactive.Banana.Frameworks

import System.IO (BufferMode(..), hSetEcho, hSetBuffering, stdin)

import Control.Monad (forever)
import Data.Char (toUpper)
import qualified Data.Map as M

type Octave = Int
data Pitch = PA | PB | PC | PD | PE | PF | PG deriving (Eq,Ord,Enum)

data Note = Note Octave Pitch deriving (Eq,Ord)

charPitches :: M.Map Char Pitch
charPitches = M.fromList $ zip ['a'..'g'] [PA .. PG]

pitchChars :: M.Map Pitch Char
pitchChars = M.fromList [ (b,a) | (a,b) <- M.toList charPitches ]

instance Show Pitch where
  show p = case M.lookup p pitchChars of
    Nothing -> error "no way this will happen"
    Just c  -> [toUpper c]

instance Show Note where
  show (Note o p) = show p ++ show o

filterMapJust :: (a -> Maybe b) -> Event t a -> Event t b
filterMapJust f = filterJust . fmap f

changeOctave :: Int -> Octave -> Octave
changeOctave d = max 0 . min 10 . (d+)

getOctaveChange :: Char -> Maybe Int
getOctaveChange c = case c of
  '+' -> Just 1
  '-' -> Just (-1)
  _   -> Nothing

setupNetwork :: Frameworks t => AddHandler Char -> Moment t ()
setupNetwork addKeyEvent = do
  eKey <- fromAddHandler addKeyEvent
  let eOctaveChange = filterMapJust getOctaveChange eKey
      bOctave = accumB 3 (changeOctave <$> eOctaveChange)
      ePitch = filterMapJust (`M.lookup` charPitches) eKey
      bPitch = stepper PC ePitch
      bNote = Note <$> bOctave <*> bPitch
  eNoteChanged <- changes bNote
  reactimate $ (\n -> putStrLn ("Now playing " ++ show n))
    <$> eNoteChanged

main :: IO ()
main = do
  (addKeyEvent, fireKey) <- newAddHandler
  let networkDescr = setupNetwork addKeyEvent
  network <- compile networkDescr
  actuate network
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  forever (getChar >>= fireKey)

