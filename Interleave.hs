{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}

module Interleave where

import Control.Monad

import qualified Data.Foldable as F
import qualified Data.Traversable as T

newtype Interleave a = Interleave
  { runInterleave :: [a]
  } deriving (Show,Functor,F.Foldable,T.Traversable)

instance Monad Interleave where
  return a = Interleave [a]
  (Interleave as) >>= f  = case as of
    []    -> mzero
    a:as' -> f a `mplus` (Interleave as' >>= f)

instance MonadPlus Interleave where
  mzero                   = Interleave []
  (Interleave as) `mplus` m2 = case as of
    []    -> m2
    a:as' -> Interleave (a : runInterleave rest)
      where rest = m2 `mplus` (Interleave as')

