{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Reflection

newtype O a s  = O { runO :: a }

-- A dictionary describing an 'Ord' instance.
newtype Ord_ a = Ord_ { compare_ :: a -> a -> Ordering }

-- Utility
isEq :: Ordering -> Bool
isEq EQ = True
isEq _  = False

instance Reifies s (Ord_ a) => Eq (O a s) where
  a == b = isEq (compare_ (reflect a) (runO a) (runO b))

instance (Eq (O a s), Reifies s (Ord_ a)) => Ord (O a s) where
  compare a b = compare_ (reflect a) (runO a) (runO b)

-- Dynamically construct an 'Ord' instance out of a comparsion operator.
withOrd :: (a -> a -> Ordering) -> (forall s. Reifies s (Ord_ a) => O a s) -> a
withOrd f v = reify (Ord_ f) (runO . asProxyOf v)
  where
  asProxyOf :: f s -> Proxy s -> f s
  asProxyOf v _ = v

-- Regular ord instance
example1 :: Int
example1 = withOrd compare $ max (O 1) (O 2)

-- Backwards ord instance
example2 :: Int
example2 = withOrd (flip compare) $ max (O 1) (O 2)

main :: IO ()
main = print example1 >> print example2

