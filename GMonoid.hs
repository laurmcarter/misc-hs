{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module GMonoid ((|<|),(|-|),(|>|)) where

import Data.Monoid

data Nil = Nil

instance Monoid Nil where
  mempty = Nil
  mappend _ _ = Nil

(|>|) :: (Monoid m1, Monoid m2) => (t -> m1) -> (t -> m2) -> [t] -> (m1,(m2,Nil))
f |>| g = f |-| end g
infixl 1 |>|

end :: (Monoid m) => (t -> m) -> [t] -> (m,Nil)
(end f) xs = (mconcat $ map f xs, Nil)

(|-|) :: (Monoid m1, Monoid m2) => (t -> m1) -> ([t] -> m2) -> [t] -> (m1,m2)
(f |-| g) xs = (mconcat $ map f xs, g xs)
infixr 0 |-|

class Builder tup typ where
  type Con tup typ
  tupApp :: Con tup typ -> tup -> typ

instance Builder Nil typ where
  type Con Nil typ = typ
  tupApp f _ = f

instance (Builder tup typ) => Builder (a,tup) typ where
  type Con (a,tup) typ = a -> Con tup typ
  tupApp f (a,tup) = tupApp (f a) tup

(|<|) :: Builder tup typ => Con tup typ -> ([typ] -> tup) -> [typ] -> typ
f |<| g = tupApp f . g
infixr 0 |<|

data Foo = Foo
  { foo  :: [Bool]
  , bar  :: [Int]
  , baz  :: [Char]
  , quux :: [Double]
  } deriving (Eq,Show)

instance Monoid Foo where
  mempty = mconcat []
  mappend m1 m2 = mconcat [m1,m2]
  mconcat = Foo |<| foo |-| bar |-| baz |>| quux

{-
data Bar = Bar
  { foo' :: [Bool]
  , bar' :: [Int]
  } deriving (Eq,Show)

instance Monoid Bar where
  mempty = mconcat []
  mappend m1 m2 = mconcat [m1,m2]
  mconcat = Bar |<| foo' |>| bar'
-}

