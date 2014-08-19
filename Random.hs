{-# LANGUAGE LambdaCase #-}

module Random 
  ( RandomM
  , runRandom
  , random
  , randoms
  , randomR
  , randomRs
  , Weight ()
  , weight
  , fromWeight
  , checkWeight
  , bool
  , index
  , hist
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State

import qualified Data.Map as M

import System.Random hiding (random, randoms, randomR, randomRs)
import qualified System.Random as R

type RandomM = State StdGen

runRandom :: RandomM a -> IO a
runRandom m = evalState m <$> newStdGen

liftRandom :: (Random a) => (StdGen -> (a,StdGen)) -> RandomM a
liftRandom f = do
  (a,g') <- f <$> get
  put g'
  return a

liftRandoms :: (Random a) => (StdGen -> [a]) -> RandomM [a]
liftRandoms f = do
  (g,g') <- split <$> get
  put g'
  return $ f g

random :: (Random a) => RandomM a
random = liftRandom R.random

randomR :: (Random a) => (a,a) -> RandomM a
randomR = liftRandom . R.randomR

randoms :: (Random a) => RandomM [a]
randoms = liftRandoms R.randoms

randomRs :: (Random a) => (a,a) -> RandomM [a]
randomRs = liftRandoms . R.randomRs

newtype Weight = W Double deriving (Eq,Ord,Show)

instance Random Weight where
  random g = let (w,g') = R.randomR (0,1) g in (W w, g')
  randomR (lo,hi) g = let
    (W lo') = checkWeight lo
    (W hi') = checkWeight hi
    (w,g') = R.randomR (lo',hi') g
    in
    (W w,g')

fromWeight :: Weight -> Double
fromWeight (W w) = w

weight :: Double -> Weight
weight = checkWeight . W

checkWeight :: Weight -> Weight
checkWeight (W w)
  | w >= 0 && w <= 1 = W w
  | True = error ("invalid weight: " ++ show w)

bool :: Weight -> RandomM Bool
bool w' = do
  w <- random
  return (w <= w')

index :: [Weight] -> RandomM Int
index ws = do
  w <- random
  let ws' = buildRange ws
  return $ findInRange w ws'

findInRange :: Weight -> [Weight] -> Int
findInRange w = flip foldl 0 $ \i w' -> if w' >= w then i else i + 1

buildRange :: [Weight] -> [Weight]
buildRange ws = let res = scanl (\(W x) (W y) -> W (x + y)) (W 0) ws in
  if last res /= W 1.0
  then error ("incomplete range: " ++ show ws)
  else tail res

test :: RandomM Bool
test = bool $ weight 0.7

hist :: (Ord a) => Int -> RandomM a -> RandomM (M.Map a Int)
hist n m = foldr (M.alter results) M.empty <$> replicateM n m
  where
  results = Just . maybe 1 (+1)


