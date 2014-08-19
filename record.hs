{-# LANGUAGE Rank2Types #-}

data Foo a = Foo { bar :: Enum a => a }

instance (Enum a, Show a) => Show (Foo a) where
  show (Foo x) = show x

