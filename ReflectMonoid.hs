{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Reflection
import Data.Monoid

-- Dynamically constructed monoid values.
data Monoid_ a = Monoid_ { mappend_ :: a -> a -> a, mempty_ :: a }

    -- Monoids which explicitly hold their own dictionary
newtype Mon1 a = Mon1 (Monoid_ a -> a)
instance Monoid (Mon1 a) where
  mempty                  = Mon1 mempty_
  Mon1 f `mappend` Mon1 g = Mon1 $ \m -> mappend_ m (f m) (g m)

embed1 :: a -> Mon1 a
embed1 x = Mon1 (\_ -> x)

run1 :: (a -> a -> a) -> a -> Mon1 a -> a
run1 f z (Mon1 v) = v (Monoid_ f z)

-- Monoids with a reflection-managed dictionary.
newtype Mon2 a s = Mon2 { unMon2 :: a }
instance Reifies s (Monoid_ a) => Monoid (Mon2 a s) where
  mappend a b        = Mon2 $ mappend_ (reflect a) (unMon2 a) (unMon2 b)
  mempty = a where a = Mon2 $ mempty_ (reflect a)

embed2 :: a -> Mon2 a s
embed2 x = Mon2 x
 
run2 :: (a -> a -> a) -> a -> (forall s. Reifies s (Monoid_ a) => Mon2 a s) -> a
run2 f z v = reify (Monoid_ f z) (unMon2 . asProxyOf v)
  where
  asProxyOf :: f s -> Proxy s -> f s
  asProxyOf v _ = v

-- Examples
sixteen :: Monoid m => m -> m
sixteen one = eight <> eight
  where
  eight   = four <> four
  four    = two  <> two
  two     = one  <> one
{-# INLINE sixteen #-}

ex1 :: Int
ex1 = run1 (+) 0 (sixteen $ embed1 2)

ex2 :: Int
ex2 = run2 (+) 0 (sixteen $ embed2 2)

ex3 :: Int
ex3 = run2 (*) 2 (sixteen mempty)

main :: IO ()
main = print ex1 >> print ex2 >> print ex3

