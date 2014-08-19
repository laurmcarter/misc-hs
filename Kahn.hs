{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Kahn where

import Data.Semigroupoid
import Data.Semigroupoid.Dual
import Control.Category
import Prelude hiding ((.),id)
import qualified Prelude as P

import Data.Semigroup hiding (Dual(..))
import Data.Monoid hiding ((<>), Dual(..))

import Data.Profunctor
import Data.Profunctor.Lift
import Data.Profunctor.Rift
import Data.Functor.Contravariant

import Control.Lens
import GHC.Exts (Constraint)

-- L {{{

-- partially applied contramap?
newtype L r a b = L
  { runL :: (r -> b) -> a
  }

instance Semigroup r => Semigroupoid (L r) where
  g `o` f = L $ \k -> runL f $ \a -> runL g $ \b -> k $ a <> b

instance Monoid r => Category (L r) where
  id    = L $ \k -> k mempty
  g . f = L $ \k -> runL f $ \a -> runL g $ \b -> k $ a `mappend` b

instance Contravariant (L r a) where
  contramap f rba = L $ \k -> runL rba $ f . k

instance Functor (Dual (L r) a) where
  fmap f p = Dual $ L $ f . runL (getDual p)

-- }}}

-- R {{{

-- partially applied fmap?
newtype R r a b = R
  { runR :: (r -> a) -> b
  }

instance Semigroup r => Semigroupoid (R r) where
  g `o` f = R $ \k -> runR g $ \b -> runR f $ \a -> k $ b <> a

instance Monoid r => Category (R r) where
  id    = R $ \k -> k mempty
  g . f = R $ \k -> runR g $ \b -> runR f $ \a -> k $ b `mappend` a

instance Functor (R r a) where
  fmap f rab = R $ f . runR rab

instance Contravariant (Dual (R r) a) where
  contramap f p = Dual $ R $ \k -> runR (getDual p) $ f . k

-- }}}

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''Dual

-- KL {{{

-- Lift :: (forall x. p b x -> p a x) -> Lift p q a b

newtype KL r a b = KL
  { runKL :: (r,b) -> (r,a)
  }

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''KL

type KL_ = Lift (,) (,)

instance Category (KL r) where
  id    = KL id
  -- (.) = underT2 L_KL (flip (.) :: ((r,c) -> (r,b)) -> ((r,b) -> (r,a)) -> (r,c) -> (r,a))
  g . f = KL $ runKL f . runKL g

instance Semigroupoid (KL r) where
  g `o` f = KL $ runKL f . runKL g

instance Contravariant (KL r a) where
  contramap f rbra = KL $ runKL rbra . fmap f

instance Functor (Dual (KL r) a) where
  fmap f p = Dual $ KL $ fmap f . runKL (getDual p)

underT :: (IsoTag tg iso, iso s t a b) => tg -> (t -> s) -> b -> a
underT = under . isoTag

underT2 :: forall tg iso s t u a b c.
  ( IsoTag tg iso , S iso s a , S iso t b , S iso u c
  ) => tg -> (s -> t -> u) -> a -> b -> c
underT2 t f a b = f (a ^. from (isoTag t :: Iso' s a)) (b ^. from (isoTag t :: Iso' t b))
  ^. (isoTag t :: Iso' u c)

type S (cls :: * -> * -> * -> * -> Constraint) s a = cls s s a a

class IsoTag (tg :: *) (cls :: * -> * -> * -> * -> Constraint) | tg -> cls where
  isoTag  :: cls s t a b => tg -> Iso s t a b
  fromTag :: cls s t a b => tg -> Iso b a t s
  fromTag = from . isoTag

data L_KL = L_KL
class KLIso s t a b | s -> a, t -> b, s b -> t, t a -> s where
  kl :: Iso s t a b

instance IsoTag L_KL KLIso where
  isoTag _ = kl

instance KLIso ((s,b) -> (s,a)) ((t,d) -> (t,c)) (KL s a b) (KL t c d) where
  kl = _KL

(^%) :: s -> LensLike (Const a) s t a b -> a
s ^% l = s ^. coerced l
infixl 8 ^%

(^.%) :: b -> AnIso s t a b -> t
b ^.% l = b ^. coerced (from l)
infixl 8 ^.%

-- }}}

-- KR {{{

-- Rift :: (forall x. p x a -> q x b) -> Rift p q a b

newtype KR r a b = KR
  { runKR :: (r -> a) -> r -> b
  }

makeLensesWith
  ( isoRules
  & lensIso .~ (Just . ('_' :))
  ) ''KR

instance Category (KR r) where
  id = KR id
  g . f = KR $ runKR g . runKR f

instance Semigroupoid (KR r) where
  g `o` f = KR $ runKR g . runKR f

instance Functor (KR r a) where
  fmap f rarb = KR $ fmap f . runKR rarb

instance Contravariant (Dual (KR r) a) where
  contramap f p = Dual $ KR $ runKR (getDual p) . fmap f

-- }}}

