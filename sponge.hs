{-# LANGUAGE KindSignatures, DataKinds, GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

--------------------
-- Naturals

data Nat = Z | S Nat deriving (Eq,Ord,Show)

class Add (a :: Nat) (b :: Nat) (ab :: Nat) | a b -> ab, a ab -> b
instance Add Z b b
instance (Add a b ab) => Add (S a) b (S ab)

-- NB: Requires UndecidableInstances
--type family Mul (a :: Nat) (b :: Nat) :: Nat
--type instance Mul Z b = Z
--type instance Mul (S n) b = Add b (Mul n b)

--------------------
-- Length-Typed Vectors

data Vec :: (* -> Nat -> *) where
  VNil :: Vec a 'Z
  VCons :: a -> Vec a n -> Vec a ('S n)

vfoldl :: (a -> b -> a) -> a -> Vec b n -> a
vfoldl f a v = case v of
  VNil       -> a
  VCons b v' -> vfoldl f (f a b) v'

vfoldr :: (a -> b -> b) -> b -> Vec a n -> b
vfoldr f b v = case v of
  VNil       -> b
  VCons a v' -> f a (vfoldr f b v')

vmap :: (a -> b) -> Vec a n -> Vec b n
vmap f v = case v of
  VNil       -> VNil
  VCons a v' -> VCons (f a) $ vmap f v'

vZipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
vZipWith f v1 v2 = case (v1,v2) of
  (VCons a v1',VCons b v2') -> VCons (f a b) $ vZipWith f v1' v2'
  (VNil,VNil) -> VNil

vappend :: Add n1 n2 n3 => Vec a n1 -> Vec a n2 -> Vec a n3
vappend v1 v2 = case v1 of
  VNil        -> v2
  VCons a v1' -> VCons a $ vappend v1' v2

-- NB: Requires Flexible{Instances,Contexts}
instance Show (Vec a Z) where
  show VNil = "[]"

instance (Show (Vec a n), Show a) => Show (Vec a (S n)) where
  show (VCons a v) = "(" ++ show a ++ " : " ++ show v ++ ")"

--------------------
-- Bits

data Bit = O | I deriving (Eq,Ord,Show)
type BitVec = Vec Bit

--------------------
-- Helpers

(.&.) :: BitVec n -> BitVec n -> BitVec n
(.&.)  = vZipWith bAnd

(.|.) :: BitVec n -> BitVec n -> BitVec n
(.|.)  = vZipWith bOr

(.^.) :: BitVec n -> BitVec n -> BitVec n
(.^.)  = vZipWith bXor

-- bitwise and
bAnd :: Bit -> Bit -> Bit
x `bAnd` y = case (x,y) of
  (O,O) -> O
  (O,I) -> O
  (I,O) -> O
  (I,I) -> I

-- bitwise or
bOr :: Bit -> Bit -> Bit
x `bOr` y = case (x,y) of
  (O,O) -> O
  (O,I) -> I
  (I,O) -> I
  (I,I) -> I

-- bitwise xor
bXor :: Bit -> Bit -> Bit
x `bXor` y = case (x,y) of
  (O,O) -> O
  (O,I) -> I
  (I,O) -> I
  (I,I) -> O

vInsertWith :: (a -> a -> Bool) -> a -> Vec a n -> Vec a (S n)
vInsertWith r a v = case v of
  VNil                   -> VCons a VNil
  VCons b v' | r a b     -> VCons a v
             | otherwise -> VCons b $ vInsertWith r a v'

vTally :: BitVec n -> Int
vTally = vfoldl f 0
  where
    f :: Int -> Bit -> Int
    f x b = case b of
      O -> x
      I -> (x + 1)

toInt :: BitVec n -> Int
toInt = vfoldl f 0
  where
    f :: Int -> Bit -> Int
    f x b = case b of
      O -> 2 * x
      I -> (2 * x) + 1

--------------------
-- Test Data

zero     = VCons O $ VCons O $ VCons O $ VCons O VNil
one      = VCons O $ VCons O $ VCons O $ VCons I VNil
two      = VCons O $ VCons O $ VCons I $ VCons O VNil
three    = VCons O $ VCons O $ VCons I $ VCons I VNil
four     = VCons O $ VCons I $ VCons O $ VCons O VNil
five     = VCons O $ VCons I $ VCons O $ VCons I VNil
six      = VCons O $ VCons I $ VCons I $ VCons O VNil
seven    = VCons O $ VCons I $ VCons I $ VCons I VNil
eight    = VCons I $ VCons O $ VCons O $ VCons O VNil
nine     = VCons I $ VCons O $ VCons O $ VCons I VNil
ten      = VCons I $ VCons O $ VCons I $ VCons O VNil
eleven   = VCons I $ VCons O $ VCons I $ VCons I VNil
twelve   = VCons I $ VCons I $ VCons O $ VCons O VNil
thirteen = VCons I $ VCons I $ VCons O $ VCons I VNil
fourteen = VCons I $ VCons I $ VCons I $ VCons O VNil
fifteen  = VCons I $ VCons I $ VCons I $ VCons I VNil
