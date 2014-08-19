{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

import Data.Function (on)
import Data.Maybe

import Matrix

import qualified Random as R
import qualified Data.List as L
import qualified Data.Map as M

type Chain  n = StateT MCState (Markov n)
type Markov n = ReaderT (TransMatrix n) R.RandomM

newtype MCState = MC Int

type TransMatrix n = Matrix R.Weight n
type Rules n = Vec R.Weight n

random :: Chain n R.Weight
random = lift $ lift $ R.random

rules :: Chain n (TransMatrix n)
rules = lift ask

mcState :: Chain n Int
mcState = do
  MC n <- get
  return n

currentRules :: Chain n (Rules n)
currentRules = vindex <$> rules <*> mcState

buildRange :: Rules :-> Rules
buildRange ws =
  let res = (vscanl (\x y -> R.weight (R.fromWeight x + R.fromWeight y)) (R.weight 0) ws)
  in case res of
    VCons _ r -> if vlast res /= R.weight 1.0
      then error ("incomplete range: " ++ show ws)
      else vtail res

findInRange :: ToSN n => R.Weight -> Rules n -> MCState
findInRange w v = MC $ fromMaybe (error "wut") $ vfind (>= w) v

index :: ToSN n => Rules n -> Chain n MCState
index v = do
  w <- random
  let v' = buildRange v
  return $ findInRange w v'

nextState :: R.Weight -> Rules n -> MCState
nextState w m = vfold

{- 
tick :: Chain MCState
tick = do
  s' <- nextState <$> random <*> currentRules
  put s'
  return s'

run :: TransMatrix -> MCState -> Chain a -> IO a
run rm s m = R.runRandom $ flip runReaderT rm $ evalStateT m s

withRules :: TransMatrix -> Chain a -> Chain a
withRules rm m = StateT $ \s -> ReaderT $ \_ -> runReaderT (runStateT m s) rm

test :: Int -> Chain (M.Map MCState Double)
test n = (fmap $ fmap (/ (fromRational $ toRational n))) <$> flip withRules
  (foldr (M.alter result) M.empty <$> replicateM n tick) $
  M.fromList $
    [ ( MC 1, M.fromList $
        [ (MC 1, R.weight 0.9)
        , (MC 2, R.weight 0.075)
        , (MC 3, R.weight 0.025)
        ])
    , ( MC 2, M.fromList $
        [ (MC 1, R.weight 0.15)
        , (MC 2, R.weight 0.8)
        , (MC 3, R.weight 0.05)
        ])
    , ( MC 3, M.fromList $
        [ (MC 1, R.weight 0.25)
        , (MC 2, R.weight 0.25)
        , (MC 3, R.weight 0.5)
        ])
    ]
  where
  result :: Maybe Double -> Maybe Double
  result = Just . maybe 1 (+1)
-}

