
import Control.Applicative
import Data.List
import qualified Data.Map as M
import System.Random

data Huffman a
  = Final2 a a
  | More a (Huffman a)
  deriving Show

huffmanEncoding :: (Eq a,Ord a) => [a] -> Huffman a
huffmanEncoding ls = mkHuffman as
  where
  hist = M.toList $ buildHist ls
  as = map fst $ sortBy (compare `on` snd) hist

mkHuffman :: [a] -> Huffman a
mkHuffman as = case as of
  [a,a'] -> Final2 a a'
  [] -> error "insufficient elements for huffman encoding"
  a:as' -> More a $ mkHuffman as'

buildHist :: (Eq a,Ord a) => [a] -> M.Map a Integer
buildHist = flip foldl M.empty $ \m a -> M.alter incr a m

incr :: Maybe Integer -> Maybe Integer
incr = Just . maybe 1 (+ 1)

data DNA
  = A
  | C
  | G
  | T
  deriving (Eq,Ord,Show)

fromDNA :: DNA -> Int
fromDNA dna = case dna of
  A -> 0
  C -> 1
  G -> 2
  T -> 3

toDNA :: Int -> DNA
toDNA n = case n `mod` 4 of
  0 -> A
  1 -> C
  2 -> G
  3 -> T

instance Random DNA where
  random  g       = let (n,g') = randomR (0 :: Int,3 :: Int) g
                      in
                      (toDNA n ,g')
  randomR (l,h) g = let (n,g') = randomR (fromDNA l,fromDNA h) g
                      in
                      (toDNA n, g')

testHuffman :: Huffman DNA
testHuffman = More A $ More C $ Final2 G T

makeTestDNA :: Int -> IO [DNA]
makeTestDNA n = do
  g <- newStdGen
  return $ take n $ randoms g

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(op `on` f) x y = f x `op` f y

encodeSequence :: Eq a => Huffman a -> [a] -> [Bool]
encodeSequence h = encodeSequence' h h
decodeSequence :: Huffman a -> [Bool] -> [a]
decodeSequence h = decodeSequence' h h

encodeSequence' :: Eq a => Huffman a -> Huffman a -> [a] -> [Bool]
encodeSequence' _ _ [] = []
encodeSequence' h lh as@(a:as') = case lh of
  More a' lh' | a == a'   -> False : encodeSequence' h h   as'
              | otherwise -> True  : encodeSequence' h lh' as
  Final2 l r  | a == l    -> False : encodeSequence' h h   as'
              | a == r    -> True  : encodeSequence' h h   as'
              | otherwise -> error "unencodable"

decodeSequence' :: Huffman a -> Huffman a -> [Bool] -> [a]
decodeSequence' _ _ [] = []
decodeSequence' h lh (False:s) = case lh of
  More a _   -> a : decodeSequence' h h s
  Final2 a _ -> a : decodeSequence' h h s
decodeSequence' h lh (True:s) = case lh of
  More _ lh' ->     decodeSequence' h lh' s
  Final2 _ a -> a : decodeSequence' h h s

