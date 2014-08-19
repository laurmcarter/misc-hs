{-# LANGUAGE TypeOperators #-}

module Join where

import Control.Applicative

join1_1 :: (b -> c) -> (a -> b) -> a -> c
--join1_1 c f x = c (f x)
join1_1 = (.)

join1_2 :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
join1_2 c f g = c <$> f <*> g

join1_3 :: (b -> c -> d -> e) -> (a -> b) -> (a -> c) -> (a -> d) -> a -> e
join1_3 c f g h = c <$> f <*> g <*> h

join1_4 :: (b -> c -> d -> e -> f) -> (a -> b) -> (a -> c) -> (a -> d) -> (a -> e) -> a -> f
join1_4 c f g h i = c <$> f <*> g <*> h <*> i

join2_1 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
--join2_1 c f x y = c (f x y)
join2_1 = (.) . (.)

join2_2 :: (c -> d -> e) -> (a -> b -> c) -> (a -> b -> d) -> a -> b -> e
join2_2 c f g = curry $ c <$> uncurry f <*> uncurry g

join2_3 :: (c -> d -> e -> f) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e) -> a -> b -> f
join2_3 c f g h = curry $ c <$> uncurry f <*> uncurry g <*> uncurry h

join2_4 :: (c -> d -> e -> f -> g) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e) -> (a -> b -> f) -> a -> b -> g
join2_4 c f g h i = curry $ c <$> uncurry f <*> uncurry g <*> uncurry h <*> uncurry i

join3_1 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
--join3_1 c f x y z = c (f x y z)
join3_1 = (.) . (.) . (.)

join3_2 :: (d -> e -> f) -> (a -> b -> c -> d) -> (a -> b -> c -> e) -> a -> b -> c -> f
join3_2 c f g = curry3 $ c <$> uncurry3 f <*> uncurry3 g

join3_3 :: (d -> e -> f -> g) -> (a -> b -> c -> d) -> (a -> b -> c -> e) -> (a -> b -> c -> f) -> a -> b -> c -> g
join3_3 c f g h x y z = c (f x y z) (g x y z) (h x y z)

join3_4 :: (d -> e -> f -> g -> h) -> (a -> b -> c -> d) -> (a -> b -> c -> e) -> (a -> b -> c -> f) -> (a -> b -> c -> g) -> a -> b -> c -> h
join3_4 c f g h i x y z = c (f x y z) (g x y z) (h x y z) (i x y z)

join4_1 :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
--join4_1 c f w x y z = c (f w x y z)
join4_1 = (.) . (.) . (.) . (.)

join4_2 :: (e -> f -> g) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f) -> a -> b -> c -> d -> g
join4_2 c f g w x y z = c (f w x y z) (g w x y z)

join4_3 :: (e -> f -> g -> h) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f) -> (a -> b -> c -> d -> g) -> a -> b -> c -> d -> h
join4_3 c f g h w x y z = c (f w x y z) (g w x y z) (h w x y z)

join4_4 :: (e -> f -> g -> h -> i) -> (a -> b -> c -> d -> e) -> (a -> b -> c -> d -> f) -> (a -> b -> c -> d -> g) -> (a -> b -> c -> d -> h) -> a -> b -> c -> d -> i
join4_4 c f g h i w x y z = c (f w x y z) (g w x y z) (h w x y z) (i w x y z)

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a,b,c,d)

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 f (a,b,c,d) = f a b c d

newtype F r a = F
  { runF :: r -> a
  }

instance Functor (F r) where
  fmap f (F g) = F $ f . g

instance Applicative (F r) where
  pure = F . const
  F f <*> F g = F $ \x -> f x $ g x

type G = F Int :+: F Char

newtype (f :+: g) a = Compose
  { decompose :: f (g a)
  }

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Compose x) = Compose $ fmap f <$> x

instance (Applicative f, Applicative g) => Applicative (f :+: g) where
  pure x = Compose $ pure <$> pure x
  Compose fs <*> Compose xs = Compose $ (<*>) <$> fs <*> xs

