{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Universe where

import Prelude hiding (max,min)
import GHC.TypeLits hiding (Nat(..))
import GHC.Exts (Constraint)

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

data U (x :: N)

data Type (env :: [*]) (a :: *) where
  Type  :: Nat n -> Type env (U n)
  (:->) :: Type env a
        -> ( HasType env x a
           )
        => ( Type       env  x
          -> Type (x ': env) b)
        -> Type (a -> b)
  -- Var   :: Nat n -> 

type family Index (n :: N) (as :: [k]) :: k where
  Index  Z    (a ': as) = a
  Index (S x) (a ': as) = Index x as

class TypeC a => HasType (env :: [*]) (a :: *) (b :: *) | env a -> b where
  type TypeC env a b :: Constraint
  type TypeC env a b  = ()
  typeOf :: Type env a -> Type env b

instance HasType (U n) (U (S n)) where
  typeOf (Type n) = Type $ S_ n

instance (HasType a (U m), HasType b (U n), x ~ Max m n)
  => HasType (a -> b) (U x) where
    typeOf (a :-> b) = case (typeOf a,typeOf b) of
      (Type m,Type n) -> Type $ max m n

type family Max (x :: N) (y :: N) :: N where
  Max  Z     Z    = Z
  Max (S x)  Z    = S x
  Max  Z    (S y) = S y
  Max (S x) (S y) = S (Max x y)

max :: Nat x -> Nat y -> Nat (Max x y)
max x y = case (x,y) of
  (Z_   ,Z_   ) -> Z_
  (Z_   ,S_ y') -> y
  (S_ x',Z_   ) -> x
  (S_ x',S_ y') -> S_ $ max x' y'

