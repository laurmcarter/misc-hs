{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}

-- Inequality (:/~) {{{

class Pred b
data Yes = Yes deriving (Show) ; instance Pred Yes
data No = No deriving (Show)   ; instance Pred No

class    (TypeEq x y No) => (x :: k) :/~ (y :: k)
instance (TypeEq (x :: k) (y :: k) No) => x :/~ y

class (TypeEq' () (x :: k) (y :: k) b) => TypeEq (x :: k) (y :: k) b where
instance (TypeEq' () (x :: k) (y :: k) b) => TypeEq (x :: k) (y :: k) b where

class TypeEq' q (x :: k) (y :: k) b | q x y -> b where
instance (b ~ Yes) => TypeEq' () (x :: k) (x :: k) b where
instance (b ~ No) => TypeEq' q (x :: k) (y :: k) b where

-- }}}

newtype Mu f = In (f (Mu f))

out :: Mu f -> f (Mu f)
out (In e) = e

data (f :+: g) e = Inl (f e) | Inr (g e) deriving (Show)
infixr 4 :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f cp = case cp of
    Inl e -> Inl $ fmap f e
    Inr e -> Inr $ fmap f e

class (Functor sub, Functor sup) => sub :</: sup

instance (Functor f, Functor g, f :/~ g) => f :</: g

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj cp = case cp of
    Inl e -> Just e
    Inr _ -> Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj cp = case cp of
    Inl _ -> Nothing
    Inr e -> prj e

inject :: (f :<: g) => f (Mu g) -> Mu g
inject = In . inj

match :: (g :<: f) => Mu f -> Maybe (g (Mu f))
match (In e) = prj e


