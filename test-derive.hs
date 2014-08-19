{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import DeriveMonoid

import Data.Monoid

data Foo a = Foo
  { foo :: [a]
  , bar :: [Bool]
  }
  deriving Show

data Bar
  = Bar
  | Bar'
  deriving Show

newtype Baz = Baz
  { baz :: [Int]
  } deriving Show

data Quux = Quux
  { quux :: Endo Char
  , quazar :: Sum Int
  }

deriveMonoid ''Foo
deriveMonoid ''Baz
deriveMonoid ''Quux

