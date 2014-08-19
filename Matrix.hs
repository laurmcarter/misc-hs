{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Matrix where

import Prelude hiding ((!!))

data Nat
  = Z
  | S Nat
  deriving (Eq,Show)

data Vec a n where
  VNil :: Vec a Z
  VCons :: a -> Vec a n -> Vec a (S n)

instance Show a => Show (Vec a n) where
  show VNil = "{}"
  show (VCons a v) = "(" ++ show a ++ " : " ++ show v ++ ")"

data SN (n :: Nat) where
  SZ :: Int -> SN Z
  SS :: FromNat n => Int -> SN n -> SN (S n)

class ToSN (n :: Nat) where
  toSN :: SN n
instance ToSN Z where
  toSN = SZ (fromNat (N :: N Z))
instance (FromNat n, ToSN n) => ToSN (S n) where
  toSN = SS (fromNat (N :: N n)) toSN

type f :-> g = forall a. f a -> g a

vfind' :: SN n -> (a -> Bool) -> Vec a n -> Maybe Int
vfind' n pr v = case (v,n) of
  (VNil,_)              -> Nothing
  (VCons a v', SS x n') -> if pr a
    then Just x
    else vfind' n' pr v'

vfind :: ToSN n => (a -> Bool) -> Vec a n -> Maybe Int
vfind = vfind' toSN

vhead :: Vec a (S n) -> a
vhead (VCons a _) = a

vtail :: Vec a (S n) -> Vec a n
vtail (VCons a v) = v

vinit :: Vec a (S n) -> Vec a n
vinit (VCons a v) = case v of
  VNil -> VNil
  VCons a v' -> VCons a $ vinit v

vlast :: Vec a (S n) -> a
vlast (VCons a v) = vlast' a v

vlast' :: a -> Vec a n -> a
vlast' a v = case v of
  VNil        -> a
  VCons a' v' -> vlast' a' v'

vmap :: (a -> b) -> Vec a :-> Vec b
vmap f v = case v of
  VNil -> VNil
  VCons a v' -> VCons (f a) $ vmap f v'

vfoldl :: (a -> b -> a) -> a -> Vec b n -> a
vfoldl f a v = case v of
  VNil       -> a
  VCons b v' -> vfoldl f (f a b) v'

vscanl :: (a -> b -> a) -> a -> Vec b n -> Vec a (S n)
vscanl f a v = VCons a $ case v of
  VNil       -> VNil
  VCons b v' -> vscanl f (f a b) v'

vindex :: Vec a n -> Int -> a
vindex (VCons a v) n = if n == 0
  then a
  else vindex v (n-1)

toList :: Vec a n -> [a]
toList v = case v of
  VNil -> []
  VCons a v' -> a:toList v'

type Matrix a n = Vec (Vec a n) n

mmap :: (a -> b) -> Matrix a :-> Matrix b
mmap = vmap . vmap

data N (n :: Nat) = N deriving (Eq,Show)

class LessThan (m :: Nat) (n :: Nat)
instance LessThan Z (S n)
instance (LessThan m n) => LessThan (S m) (S n)

class FromNat (n :: Nat) where
  fromNat :: N n -> Int
instance FromNat Z where
  fromNat _ = 0
instance (FromNat n) => FromNat (S n) where
  fromNat _ = 1 + fromNat (N :: N n)

(!) :: (FromNat m, LessThan m n) => Vec a n -> N m -> a
v ! n = let i = fromNat n in vindex v i

v1 = VCons 1 $ VCons 2 $ VCons 3 $ VCons 4 VNil
n1 = N :: N (S (S Z))
n2 = N :: N (S Z)

m1 = VCons (VCons 1 $ VCons 2 $ VCons 3 VNil) $
     VCons (VCons 4 $ VCons 5 $ VCons 6 VNil) $
     VCons (VCons 7 $ VCons 8 $ VCons 9 VNil) $
     VNil

(!!) :: (FromNat l, FromNat m, LessThan l n, LessThan m n) => Matrix a n -> (N l, N m) -> a
m !! (r,c) = (m ! r) ! c

