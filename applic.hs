{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative hiding (Const (..))
import Data.Monoid hiding (Sum (..), Product (..))

data Const mo a = Const mo               deriving (Show,Functor)
data Compose f g a = Compose (f (g a))   deriving (Show,Functor)
data Product f g a = Product (f a) (g a) deriving (Show,Functor)
data Sum f g a = SumL (f a) | SumR (g a) deriving (Show,Functor)

instance (Monoid mo) => Applicative (Const mo) where
  pure _ = Const mempty
  Const m <*> Const m' = Const (mappend m m')

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure = Compose . pure . pure
  Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x

instance (Applicative f, Applicative g) => Applicative (Product f g) where
  pure a = Product (pure a) (pure a)
  Product f g <*> Product x y = Product (f <*> x) (g <*> y)

instance (Applicative f, Applicative g, Natural g f) => Applicative (Sum f g) where
  pure a = SumR $ pure a
  SumL f <*> SumL x = SumL (f <*> x)
  SumR g <*> SumL x = SumL (eta g <*> x)
  SumL f <*> SumR x = SumL (f <*> eta x)
  SumR g <*> SumR y = SumR (g <*> y)

class Natural f g where
  eta :: f a -> g a

instance (Monoid mo) => Natural f (Const mo) where
  eta = const $ Const mempty

instance Applicative f => Natural g (Compose f g) where
  eta = Compose . pure

instance (Applicative g, Functor f) => Natural f (Compose f g) where
  eta = Compose . fmap pure

instance (Natural f g) => Natural f (Product f g) where
  eta x = Product x (eta x)

instance (Natural g f) => Natural g (Product f g) where
  eta x = Product (eta x) x

instance Natural (Product f g) f where
  eta (Product x _ ) = x

instance Natural (Product f g) g where
  eta (Product _ x) = x

instance Natural g f => Natural (Sum f g) f where
  eta (SumL x) = x
  eta (SumR y) = eta y


