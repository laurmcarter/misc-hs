{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DFA where

import Control.Applicative (Applicative,(<$>),(<*>))
import Control.Monad
import Control.Monad.State (State,MonadState)
import qualified Control.Monad.State as S
import Data.List

data FAS l = FAS
  { label :: l
  , acceptQ :: Bool
  , trans0 :: FA l
  , trans1 :: FA l
  }

type FA l = [FAS l]

dom18 :: FA Int
dom18 = [one]
  where
  one = FAS 1 True [one,two] []
  two = FAS 2 True [two,one] [one,two]

instance Eq l => Eq (FAS l) where
  FAS l1 _ _ _ == FAS l2 _ _ _ = l1 == l2

class Occ occ where
  empty :: occ a
  seenp :: Eq a => a -> occ a -> Bool
  put   :: a -> occ a -> occ a

instance Occ [] where
  empty = []
  seenp = elem
  put   = (:)

instance (Eq l, Show l) => Show (FAS l) where
  show st = unwords
    [ "{@"
    , intercalate ", " $ evalSeen $ showStates [st]
    , "@}"
    ]

class OccM a m | m -> a where
  seenM :: Eq a => a -> m Bool
  putM  ::         a -> m ()

instance OccM (FAS l) (Seen l) where
  seenM st = liftM (seenp st) S.get
  putM     = S.modify . put

newtype Seen l a = Seen
  { unSeen :: State (FA l) a
  } deriving
  ( Functor
  , Applicative
  , Monad , MonadState (FA l)
  )

evalSeen :: Seen l a -> a
evalSeen = flip S.evalState empty . unSeen

showStates :: (Eq l, Show l) => [FAS l] -> Seen l [String]
showStates = \case
  [] -> return []
  st@(FAS _ _ t0 t1) : rest
     -> whenM (seenM st)
          (showStates rest)
          ((showState st :) <$> (putM st >> showStates (t0++t1++rest)))
  where
  showState (FAS l acc t0 t1) = unwords
    [ "{State"
    , show l
    , show acc
    , show $ map label t0
    , show $ map label t1
    ] ++ "}"

whenM :: Monad m => m Bool -> m a -> m a -> m a
whenM mt mc ma = do
  t <- mt
  if t then mc else ma

finAuAccept :: FA l -> [Bool] -> Bool
finAuAccept sts str = foldr
  (\l seed -> acceptP l str || seed) False sts
  where
  acceptP (FAS _ acc t0 t1) = \case
    []      -> acc
    s:rest -> finAuAccept (if s then t1 else t0) rest

t1 = finAuAccept dom18 $ map (>0) [0,1,0,1]
t2 = finAuAccept dom18 $ map (>0) [1,1,0,1]
t3 = finAuAccept dom18 [True]
t4 = finAuAccept dom18 [False]

class StateDict sd where
  emptyd :: sd (l,FAS l)
  locate :: Eq l => l -> sd (l,FAS l) -> Maybe (FAS l)
  putd   :: (l,FAS l) -> sd (l,FAS l) -> sd (l,FAS l)

instance StateDict [] where
  emptyd = []
  locate = lookup
  putd   = (:)

determinize_cc :: (StateDict sd, Ord l)
  => FA l -> sd ([l], FAS [l]) -> (FA [l], sd ([l], FAS [l]))
determinize_cc states converted_states =
  -- first, check the cache to see if the state has been built already
  case locate dfa_label converted_states of
    Nothing -> build_state
    Just dfa_state -> ([dfa_state],converted_states)
  where
  -- [NFA_labels] -> DFA_labels
  det_labels = sort . nub . (map label)
  dfa_label  = det_labels states

  -- find out NFA-followers for [nfa_state] upon ingestion of 0 and 1
  (t0_followers,t1_followers) = 
    foldr (\st (f0,f1) -> (trans0 st ++ f0, trans1 st ++ f1))
        ([],[]) states
  acceptQ'    = or (map acceptQ states)
  
  -- really build the dfa state and return ([dfa_state],updated_cache)
  build_state = let
    -- note, the dfa_state is computed _below_
    converted_states1 = (dfa_label,dfa_state) `putd` converted_states
    (t0', converted_states2) = determinize_cc t0_followers converted_states1
    (t1', converted_states3) = determinize_cc t1_followers converted_states2
    dfa_state = FAS dfa_label acceptQ' t0' t1'
    in ([dfa_state],converted_states3)

