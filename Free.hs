{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}

module Free where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Reader

import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Free f a = Pure a | Free (f (Free f a))

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
  showsPrec d (Pure a) = showParen (d > up_prec) $ showString "Pure " . showsPrec (up_prec+1) a
    where
    up_prec = 10
  showsPrec d (Free as) = showParen (d > up_prec) $ showString "Free " . showsPrec (up_prec+1) as
    where
    up_prec = 10

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free as) = Free $ fmap (fmap f) as

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free as >>= f = Free $ fmap (>>= f) as

instance MonadTrans Free where
  lift = Free . liftM Pure

retract :: Monad f => Free f a -> f a
retract (Pure a) = return a
retract (Free as) = as >>= retract

class (Monad m, Functor f) => MonadFree f m | m -> f where
  wrap :: f (m a) -> m a

instance Functor f => MonadFree f (Free f) where
  wrap = Free

instance (Functor f, MonadFree f m) => MonadFree f (ReaderT e m) where
  wrap fs = ReaderT $ \e -> wrap $ fmap (`runReaderT` e) fs

newtype Codensity f a = Codensity (forall r. (a -> f r) -> f r)

runCodensity :: Codensity f a -> (a -> f r) -> f r
runCodensity (Codensity m) = m

instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity $ \k -> m (k . f)

instance Monad (Codensity f) where
  return a = Codensity $ \k -> k a
  m >>= k  = Codensity $ \c -> runCodensity m $ \a -> runCodensity (k a) c

lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity m = runCodensity m return

data Bin a = Bin a a deriving (Show,Functor,F.Foldable,T.Traversable)

type Tree = Free Bin

bin :: MonadFree Bin m => m a -> m a -> m a
bin = wrap .: Bin

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

tip :: MonadFree Bin m => a -> m a
tip = return

instance MonadFree f m => MonadFree f (Codensity m) where
  wrap t = Codensity $ \h -> wrap $ fmap (`runCodensity` h) t

improve :: Functor f => (forall m. MonadFree f m => m a) -> Free f a
improve m = lowerCodensity m

newtype Yoneda f a = Yoneda (forall r. (a -> r) -> f r)


