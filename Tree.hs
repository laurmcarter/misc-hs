{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Tree where

import Control.Applicative
import Control.Monad

data Tree r a
  = Leaf a
  | Node (r -> Tree r a)

instance Functor (Tree r) where
  fmap f = \case
    Leaf a -> Leaf $ f a
    Node g -> Node $ fmap f . g

instance Applicative (Tree r) where
  pure  = return
  (<*>) = ap

instance Monad (Tree r) where
  return = Leaf
  m >>= f = case m of
    Leaf a -> f a
    Node g -> Node $ g >=> f

instance (Bounded r, Enum r, Eq a) => Eq (Tree r a) where
  Leaf a == Leaf b = a == b
  Node f == Node g = all ((==) <$> f <*> g) [minBound .. maxBound]
  _      == _      = False

instance (Bounded r, Enum r, Show r, Show a) => Show (Tree r a) where
  showsPrec d t = showParen (d > 10) $ case t of
    Leaf a -> showString "Leaf " . showsPrec 11 a
    Node f -> showString "Node " . showList [ (r,f r) | r <- [minBound .. maxBound] ]

t0 :: Tree Bool Int
t0 = Node $ cond
   $~ Leaf 3
   $  Leaf 4

t1 :: Tree Int Int
t1 = Node $ enum
  [ Leaf 0
  , Leaf 1
  , Leaf 2
  ]
  $ Leaf 5

cond :: a -> a -> Bool -> a
cond t f b = if b then t else f

enum :: Enum i => [a] -> a -> i -> a
enum as def (fromEnum -> i)
  | i < 0         = def
  | i < length as = as !! i
  | otherwise     = def

($~) :: (a -> b) -> a -> b
f $~ x = f x
infixr 2 $~

