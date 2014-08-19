{-# LANGUAGE MultiParamTypeClasses,UndecidableInstances,FunctionalDependencies,FlexibleInstances,KindSignatures,DataKinds,GADTs #-}

data Nat = Z | S Nat

--data Z
--data S a

class Sum2 (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c, a c -> b
instance Sum2 'Z b b
instance (Sum2 a b c) => Sum2 ('S a) b ('S c)

class Sum (a :: Nat) (b :: Nat) (c :: Nat) | a b -> c, a c -> b, b c -> a
instance (Sum2 a b c, Sum2 b a c) => Sum a b c

--data Vec :: * -> Nat -> * where
--  VNil :: Vec a 'Z
--  VCons :: a -> Vec a n -> Vec a ('S n)
--
--vappend :: Sum n1 n2 n3 => Vec a n1 -> Vec a n2 -> Vec a n3
--vappend v1 v2 = case v1 of
--  VNil        -> v2
--  VCons a v1' -> VCons a (vappend v1' v2)

add :: Sum a b c => a -> b -> c
add = undefined

type One = 'S 'Z
