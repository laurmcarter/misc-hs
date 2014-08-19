{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Monoid

newtype Disj = Disj Bool deriving (Eq,Show)
newtype Conj = Conj Bool deriving (Eq,Show)

instance Monoid Disj where
  mempty = Disj False
  mappend (Disj x) (Disj y) = Disj $ x || y

instance Monoid Conj where
  mempty = Conj True
  mappend (Conj x) (Conj y) = Conj $ x && y

