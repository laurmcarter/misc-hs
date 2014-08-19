{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies, TypeOperators #-}

module LiftAn where

import Control.Applicative

data All :: (* -> *) -> [*] -> * where
  Nil  ::                    All f '[]
  (:*) :: f x -> All f xs -> All f (x ': xs)

infixr 5 :*

type family (ts :: [*]) @-> (r :: *) :: *
type instance '[]       @-> r =             r
type instance (t ': ts) @-> r = t -> ts @-> r

-- | liftA2 implemented in terms of the liftAn operation below
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f x y = liftAn f (x :* y :* Nil)

liftAn :: Applicative f => (ts @-> r) -> All f ts -> f r
liftAn = go . pure
  where
  go :: Applicative f => f (us @-> r) -> All f us -> f r
  go fr Nil = fr
  go ff (fx :* xs) = go (ff <*> fx) xs

