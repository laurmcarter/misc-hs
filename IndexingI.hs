{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Control.Lens.IndexingI where

import Control.Applicative
import Control.Lens hiding ((<.>))
import Data.Bifunctor (second)
import Data.Functor.Apply (Apply (..))



newtype IndexingI i f a = IndexingI
  { runIndexingI :: i -> (i, f a)
  }

instance Functor f => Functor (IndexingI i f) where
  fmap f m = IndexingI $ \i -> second (fmap f) $ runIndexingI m i
  {-# INLINE fmap #-}

instance Applicative f => Applicative (IndexingI i f) where
  pure x = IndexingI $ \i -> (i, pure x)
  {-# INLINE pure #-}
  IndexingI mf <*> IndexingI ma = IndexingI $ \i -> case mf i of
    (j, ff) -> case ma j of
      ~(k, fa) -> (k, ff <*> fa)
  {-# INLINE (<*>) #-}

instance Contravariant f => Contravariant (IndexingI i f) where
  contramap f (IndexingI m) = IndexingI $ \i -> case m i of
    (j, ff) -> (j, contramap f ff)
  {-# INLINE contramap #-}

instance Apply f => Apply (IndexingI i f) where
  IndexingI mf <.> IndexingI ma = IndexingI $ \i -> case mf i of
    (j, ff) -> case ma j of
      ~(k, fa) -> (k, ff <.> fa)
  {-# INLINE (<.>) #-}



indexingI :: Indexable i p => i -> (i -> i)
  -> IndexingLensLike i f s t a b
  -> p a (f b) -> s -> f t
indexingI izero isucc l iafb s =
  snd $ runIndexingI (l (\a -> IndexingI (\i -> i `seq` (isucc i, indexed iafb i a))) s) izero
{-# INLINE indexingI #-}

innerIndexingI :: Indexable i p => i -> (i -> a -> i)
  -> IndexingLensLike i f s t a b
  -> p a (f b) -> s -> f t
innerIndexingI izero isucc l iafb s =
  snd $ runIndexingI (l iafb' s) izero
  where
  iafb' a = IndexingI $ \i -> i `seq` (isucc i a, indexed iafb i a)
{-# INLINE innerIndexingI #-}

{-
-- NB: questionable usefulness
outerIndexingI :: Indexable i p => i -> (s -> i -> i)
  -> ((a -> IndexingI i f b) -> s -> IndexingI i f t)
  -> p a (f b) -> s -> f t
outerIndexingI izero isucc l iafb s = indexingI izero isucc' l iafb s
  where
  isucc' = isucc s
{-# INLINE outerIndexingI #-}
-}

-- XXX: can
--   postindexingI :: Indexable i p => i -> (i -> b -> i)
--     -> ((a -> IndexingI i f b) -> s -> IndexingI i f t)
--     -> p a (f b) -> s -> f t
-- be written?
-- perhaps instead of (i -> b -> i), it has to be (i -> f b -> i)



splits :: IndexPreservingLens (Int,[a]) (Int,[b]) ([a],[a]) ([b],[b])
splits = iplens (uncurry split) append
  where
  split  :: Int -> [a] -> ([a],[a])
  split  = splitAt
  append :: (Int,[a]) -> ([b],[b]) -> (Int,[b])
  append _ (l1,l2) = (length l1,l1 ++ l2)

{-
splits :: IndexedFold Int [a] ([a],[a])
-- splits :: IndexedLens Int [a] [b] ([a],[a]) ([b],[b])
splits = indexingI 0 (+1) splitting

splitting :: IndexingLens Int [a] [b] ([a],[a]) ([b],[b])
splitting = ixinglens (flip splitAt) (uncurry (++))
-}

ixinglens :: (s -> i -> a) -> (b -> t) -> IndexingLens i s t a b
ixinglens sia bt iafb s = IndexingI $ \i ->
  second (fmap bt) $ runIndexingI (iafb $ sia s i) i

ixinglens' :: (s -> i -> a) -> (s -> b -> t) -> IndexingLens i s t a b
ixinglens' sia sbt iafb s = IndexingI $ \i ->
  second (fmap $ sbt s) $ runIndexingI (iafb $ sia s i) i

type IndexingLens i s t a b = forall f. (Functor f) => IndexingLensLike i f s t a b
type IndexingLensLike i f s t a b = (a -> IndexingI i f b) -> s -> IndexingI i f t

-- type IndexedLens i s t a b = forall f p. (Indexable i p, Functor f) => p a (f b) -> s -> f t

-- ilens :: (s -> (i, a)) -> (s -> b -> t) -> IndexedLens i s t a b
-- ilens sia sbt iafb s = sbt s <$> uncurry (indexed iafb) (sia s)

