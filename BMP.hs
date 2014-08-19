{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}

import Codec.BMP
import Data.List (transpose)
import qualified Data.ByteString as BS
import Graphics.Gloss.Data.Picture

import Text.Show.Pretty

grid :: (Int,Int) -> (Int,Int) -> [a] -> [[[a]]]
grid (imgX,imgY) (tileX,tileY) as
  | 0 <- imgX `mod` tileX
  , 0 <- imgY `mod` tileY
  =
  | otherwise
  = 

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n as = a : chunk n rest
  where
  (a,rest) = splitAt n as

chunkBS :: Int -> BS.ByteString -> [BS.ByteString]
chunkBS n s = if
  | BS.null s1   -> []
  | BS.null rest -> [s1]
  | otherwise -> s1 : chunkBS n rest
  where
  (s1,rest) = BS.splitAt n s

display :: Show a => a -> IO ()
display = putStrLn . ppShow

