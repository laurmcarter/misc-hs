{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Prelude hiding (or,and)

-- Terms {{{

data V v a = V (v a) deriving (Eq,Show,Functor)
data (f :+: t) a
  = H (f a)
  | T (t a)
  deriving (Eq,Show,Functor)
infixr :+:

data Mu (f :: * -> *) t = Mu { runMu :: t (Mu f t) }

instance (Show (t (Mu f t))) => Show (Mu f t) where
  show = show . runMu

class (Functor v) => Var v

class (Functor t) => Term t
instance (Var v) => Term (V v)
instance (Functor f, Term t) => Term (f :+: t)

-- }}}

-- Subtype {{{

type f :-> g = forall a. f a -> g a
infixr :->

type (f :.: g) a = f (g a)
infixr :.:

inject :: (sub :<: t) => sub (Mu f t) -> Mu f t
inject = Mu . inj

class (Functor sub, Term sup) => sub :<: sup where
  inj :: sub :-> sup
  prj :: sup :-> Maybe :.: sub

instance (Var v) => V v :<: V v where
  inj = id
  prj = Just

instance (Functor f, Term t) => f :<: (f :+: t) where
  inj = H
  prj = \case
    H f -> Just f
    T _ -> Nothing

instance (Functor f, Functor g, f :<: t) => f :<: (g :+: t) where
  inj = T . inj
  prj = \case
    H _ -> Nothing
    T t -> prj t

-- }}}

data LV a = LV Integer deriving (Eq,Show,Functor)
instance Var LV
lv :: (LV :<: t) => Integer -> Mu f t
lv = inject . LV

data I a = I Integer deriving (Eq,Show,Functor)
i :: (I :<: t) => Integer -> Mu I t
i = inject . I

data Add a = Add a a deriving (Eq,Show,Functor)
add :: (Add :<: t) => Mu I t -> Mu I t -> Mu I t
add = inject .: Add

data Mul a = Mul a a deriving (Eq,Show,Functor)
mul :: (Mul :<: t) => Mu I t -> Mu I t -> Mu I t
mul = inject .: Mul

data B a = B Bool deriving (Eq,Show,Functor)
b :: (B :<: t) => Bool -> Mu B t
b = inject . B

data Or a = Or a a deriving (Eq,Show,Functor)
or :: (Or :<: t) => Mu B t -> Mu B t -> Mu B t
or = inject .: Or

data And a = And a a deriving (Eq,Show,Functor)
and :: (And :<: t) => Mu B t -> Mu B t -> Mu B t
and = inject .: And

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr .:

data LC a

data Var' a = Var' String deriving (Eq,Show,Functor)
var :: (Var' :<: t) => String -> Mu LC t
var = inject . Var'

data Lambda a = Lambda String a deriving (Eq,Show,Functor)
lambda :: (Lambda :<: t) => String -> Mu LC t -> Mu LC t
lambda = inject .: Lambda

data App a = App a a deriving (Eq,Show,Functor)
app :: (App :<: t) => Mu LC t -> Mu LC t -> Mu LC t
app = inject .: App

class (Functor t) => Algebra t a where
  alg :: t a -> a

evalAlg :: Algebra t a => Mu f t -> a
evalAlg = alg . fmap evalAlg . runMu

instance (Algebra f a, Algebra t a) => Algebra (f :+: t) a where
  alg = \case
    H f -> alg f
    T t -> alg t

instance Functor v => Algebra (V v) a where
  alg _ = error "non-ground"

instance Algebra B Bool where
  alg (B b) = b

instance Algebra Or Bool where
  alg (Or x y) = x || y

instance Algebra And Bool where
  alg (And x y) = x && y

testB :: Mu B (And :+: Or :+: B :+: V LV)
testB = and (or (b True) (or (b False) (b True))) (b False)

