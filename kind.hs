{-# LANGUAGE KindSignatures, TypeFamilies, DataKinds, PolyKinds, GADTs #-}

type family F a :: Bool
type instance F Int = True
type instance F Bool = False

data Foo (a :: Bool) where
  Foo :: a -> Foo (F a)

