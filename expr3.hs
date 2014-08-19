{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}

import Debug.Trace

-- Expr {{{1

newtype Expr f = In (f (Expr f))

foldExpr :: (Show (Expr f), Functor f) => (f a -> a) -> Expr f -> a
foldExpr f e@(In t) = trace ("foldExpr: " ++ show e ++ "\n") $ f (fmap (foldExpr f) t)

instance Show (f (Expr f)) => Show (Expr f) where
  showsPrec _ (In x) = showParen True (showString "In " . showsPrec 11 x)

out :: Expr f -> f (Expr f)
out (In e) = e

-- :+: {{{1

data (f :+: g) e = Inl (f e) | Inr (g e) deriving Show

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap h cp = case cp of
    Inl f -> Inl $ fmap h f
    Inr g -> Inr $ fmap h g

-- :<: {{{1

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance TypeTree sub sup => sub :<: sup where
  inj = treeInj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

reinject :: (Show (Expr f), f :<: g) => Expr f -> Expr g
reinject = foldExpr inject

-- Instances {{{1

data HTrue
data HFalse

class (Functor sub, Functor sup) => TypeTree sub sup where
  treeInj :: sub a -> sup a
instance Functor x => TypeTree x x where
  treeInj = id

class IsTreeMem (sub :: * -> *) (sup :: * -> *) b | sub sup -> b
instance TypeEq x y b => IsTreeMem x y b
instance (IsTreeMem x l bl, IsTreeMem x r br, TypeOr bl br b) => IsTreeMem x (l :+: r) b

class (Functor sub, Functor l, Functor r)  => TypeTree' b sub l r where
  treeInj' :: b -> sub a -> (l :+: r) a
instance (TypeTree x l, Functor r) => TypeTree' HTrue x l r where
  treeInj' _ = Inl . treeInj
instance (TypeTree x r, Functor l) => TypeTree' HFalse x l r where
  treeInj' _ = Inr . treeInj

instance (IsTreeMem x l b, TypeTree' b x l r) => TypeTree x (l :+: r) where
  treeInj = treeInj' (undefined :: b)

class TypeOr b1 b2 res | b1 b2 -> res
instance TypeOr HFalse HFalse HFalse
instance TypeOr HFalse HTrue HTrue
instance TypeOr HTrue HFalse HTrue
instance TypeOr HTrue HTrue HTrue

class TypeEq (x :: * -> *) (y :: * -> *) b | x y -> b
instance TypeEq x x HTrue
instance (b ~ HFalse) => TypeEq x y b

-- }}}

-- Arithmetic {{{

data Val e = Val Int deriving Show
data Add e = Add e e deriving Show
data Mul e = Mul e e deriving Show

instance Functor Val where
  fmap f (Val x) = Val x
instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

val :: (Val :<: f) => Int -> Expr f
val x = inject $ Val x

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = inject $ Add x y

mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = inject $ Mul x y

-- }}}
