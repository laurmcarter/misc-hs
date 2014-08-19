{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Peano where

import GHC.TypeLits
import Data.Type.Equality
import Unsafe.Coerce

data Vec (n :: Nat) a where
  Nil  :: Vec 0 a
  (:*) :: KnownNat n => a -> Vec n a -> Vec (1 + n) a
infixr 4 :*

v0 :: Vec 2 Int
v0 = 2 :* 1 :* Nil

{-
(*:*) :: Vec m a -> Vec n a -> Vec (m + n) a
v1 *:* v2 = case v1 of
  Nil      -> v2
  a :* v1' -> a :* (v1' *:* v2)
-}

data Peano (n :: Nat) where
  Z :: Peano 0
  S :: Peano (n - 1) -> Peano n

addZ :: Peano x -> (x + 0) :~: x
addZ _ = Refl

natElim :: forall p x. p 0 -> (forall x. (1 <= x) => Peano (x - 1) -> p (x - 1) -> p x) -> Peano x -> p x
natElim z s x = case x of
  Z    -> z
  S x' -> s _ _ -- x' $ natElim z s x'

newtype AddZ x   = AddZ { unAddZ :: x + 0 :~: x }

_Z :: 0 :~: 0
_Z = Refl

_S :: x :~: y -> (x + 1) :~: (y + 1)
_S Refl = Refl

(+:) :: (w :~: y) -> (x :~: z) -> (w + x) :~: (y + z)
Refl +: Refl = Refl

{-
newtype AddS x y = AddS { unAddS :: (x + (y + 1)) :~: ((x + y) + 1) }
addS :: forall x y. Peano x -> Peano y -> (x + (y + 1)) :~: ((x + y) + 1)
addS x = unAddS . go
  where
  go :: Peano y -> AddS x y
  go = natElim (AddS Refl) $ \x (AddS p@Refl) -> AddS _
-}

type family AddS_ (x :: Nat) (y :: Nat) :: * where
  AddS_ x 0 = (x + 1) :~: (x + 1)
  AddS_ x y = (AddS_ x (y-1),(x + (y + 1)) :~: ((x + y) + 1))

newtype AddS x y = AddS
  { unAddS :: AddS_ x y
  }

{-
addS :: forall x y. Peano x -> Peano y -> AddS x y
addS x = natElim (AddS Refl) $ \y (AddS p@Refl) -> AddS _
-}

{-
addComm :: Peano x -> Peano y -> (x + y) :~: (y + x)
addComm x y = 
-}

{-
addAssoc :: Peano x -> Peano y -> Peano z -> ((x + y) + z) :~: (x + (y + z))
addAssoc x y z = 
-}

