{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Control.Monad (join)
import Data.List (intercalate)

-- Nats
---------------------------------------------
data Nat = Z | S Nat deriving (Eq,Show)

type Zero = Z
type One = S Z
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four
type Six = S Five
type Seven = S Six
type Eight = S Seven
type Nine = S Eight
type Ten = S Nine

type family Prev (n :: Nat) :: Nat
type instance Prev (S n) = n

type family Sum (a :: Nat) (b :: Nat) :: Nat
type instance Sum Z b = b
type instance Sum (S a) b = S (Sum a b)

class NotZero (n :: Nat)
instance NotZero (S n)

data Less :: Nat -> Nat -> * where
  LessZ :: Less Z (S n)
  LessS :: Less m n -> Less (S m) (S n)

---------------------------------------------

-- Vectors
---------------------------------------------
data Vec :: Nat -> * -> * where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

instance Functor (Vec n) where
  fmap f VNil = VNil
  fmap f (VCons a v) = VCons (f a) $ fmap f v

instance Applicative (Vec Z) where
  pure x = VNil
  v1 <*> v2 = VNil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure x = VCons x $ pure x
  (VCons f v1) <*> (VCons a v2) = VCons (f a) (v1 <*> v2)

--transpose :: (Applicative (Vec m), Applicative (Vec n)) => Vec m (Vec n a) -> Vec n (Vec m a)
--transpose VNil = pure VNil
--transpose (VCons v vs) = let vs' = transpose vs

zipWithF :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
zipWithF f a b = f <$> a <*> b

instance Show a => Show (Vec Z a) where
  show VNil = "{}"

instance (Show a, Show (Vec n a)) => Show (Vec (S n) a) where
  show (VCons a v) = let vs = show v in
    case vs of
      "{}" -> '{' : show a ++ "}"
      _    -> '{' : show a ++ "," ++ tail vs

vhead :: NotZero n => Vec n a -> a
vhead (VCons a _) = a

vtail :: NotZero n => Vec n a -> Vec (Prev n) a
vtail (VCons _ v) = v

vappend :: Vec m a -> Vec n a -> Vec (Sum m n) a
vappend v1 v2 = case v1 of
  VNil        -> v2
  VCons a v1' -> VCons a $ vappend v1' v2

vsum :: Ring a => Vec n a -> a
vsum VNil        = idAdd
vsum (VCons a v) = a `add` vsum v

vproduct :: Ring a => Vec n a -> a
vproduct VNil        = idMul
vproduct (VCons a v) = a `mul` vproduct v

vadd :: (Applicative (Vec n), Num a) => Vec n a -> Vec n a -> Vec n a
vadd = zipWithF (+)

vmul :: (Applicative (Vec n), Num a) => Vec n a -> Vec n a -> Vec n a
vmul = zipWithF (*)

toList :: Vec n a -> [a]
toList VNil = []
toList (VCons a v) = a : toList v

v1,v2 :: Vec Three Int
v1 = VCons 1 $ VCons 2 $ VCons 3 VNil
v2 = VCons 4 $ VCons 5 $ VCons 6 VNil
---------------------------------------------

---------------------------------------------
data SMatrix n a = SM { unSM :: Vec n (Vec n a) }

type M n = SMatrix n Int

instance (Show a, Show (Vec n a)) => Show (SMatrix n a) where
  show (SM vv) = case vv of
    VNil -> "||"
    _    -> intercalate "\n" $ map ((\s -> "|"++s++"|") . tail . init . show) $ toList vv

instance Functor (SMatrix n) where
  fmap f (SM vv) = SM $ fmap (fmap f) vv

instance Applicative (Vec n) => Applicative (SMatrix n) where
  pure x    = SM $ pure $ pure x
  (SM v1) <*> (SM v2) = SM $ zipWithF (<*>) v1 v2

mheadCol :: NotZero n => SMatrix n a -> Vec n a
mheadCol (SM vv) = fmap vhead vv

mheadRow :: NotZero n => SMatrix n a -> Vec n a
mheadRow (SM vv) = vhead vv

mtail :: NotZero n => SMatrix n a -> SMatrix (Prev n) a
mtail (SM vv) = SM $ fmap vtail $ vtail vv

--mmul :: Ring a => SMatrix n a -> SMatrix n a -> SMatrix n a
--mmul (SM v1) (SM v2) = 

---------------------------------------------

---------------------------------------------
class Ring a where
  idAdd :: a
  idMul :: a
  add   :: a -> a -> a
  mul   :: a -> a -> a

instance Ring Int where
  idAdd = 0
  idMul = 1
  add   = (+)
  mul   = (*)

instance (Applicative (Vec n), Ring a) => Ring (Vec n a) where
  idAdd = pure idAdd
  idMul = pure idMul
  add   = zipWithF add
  mul   = zipWithF mul

instance Ring a => Ring (SMatrix Z a) where
  idAdd = SM VNil
  idMul = SM VNil
  add (SM v1) (SM v2) = SM $ zipWithF (zipWithF add) v1 v2
  mul (SM v1) (SM v2) = SM $ zipWithF (zipWithF mul) v1 v2

instance (Applicative (Vec n), Ring a, Ring (SMatrix n a)) => Ring (SMatrix (S n) a) where
  idAdd = SM $ pure $ pure idAdd
  idMul = SM $ VCons (VCons idMul $ pure idAdd) $ fmap (VCons idAdd) $ unSM idMul
  add (SM v1) (SM v2) = SM $ zipWithF (zipWithF add) v1 v2
  mul (SM v1) (SM v2) = SM $ zipWithF (zipWithF mul) v1 v2

