{-# LANGUAGE RebindableSyntax #-}

import Prelude hiding ((>>=), (>>), return, fmap, fail)
import IndexedState

import Data.Char

myIStateComp :: IState Int Char Int
myIStateComp = do
  original <- get
  modify chr
  return original
