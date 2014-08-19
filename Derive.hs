
-- Extensions {{{
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
-- }}}

module Derive where

import Language.Haskell.TH
import qualified Text.Show.Pretty as P

import Control.Applicative
import Control.Arrow
import Control.Monad

import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- Fix / Cata / Ana {{{

data Fix f = Fix { unFix :: f (Fix f) }

instance (Show (f (Fix f))) => Show (Fix f) where
  show (Fix f) = "(Fix " ++ show f ++ ")"

class (Functor f) => Cata f t | f -> t where
  cataF :: Cata g t => f (Fix g) -> t
  cata  :: Fix f -> t
  cata   = unFix >>> cataF

class (Functor f) => Ana t f | t -> f where
  anaF  :: Ana t f => t -> f (Fix f)
  ana   :: t -> Fix f
  ana    = Fix <<< anaF

data (f :+: g) a = L (f a) | R (g a) deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

instance (Cata f t, Cata g t) => Cata (f :+: g) t where
  cataF = \case
    L e -> cataF e
    R e -> cataF e

instance (Ana t f, Ana u f) => Ana (Either t u) f where
  anaF = \case
    Left e  -> anaF e
    Right e -> anaF e

foldFix :: Functor f => (f a -> a) -> Fix f -> a
foldFix f = f . fmap (foldFix f) . unFix

foldTerm :: (Ana t f) => (f a -> a) -> t -> a
foldTerm f = foldFix f . ana

foldFixM :: (Monad m, T.Traversable f) => (f a -> m a) -> Fix f -> m a
foldFixM f (Fix e) = f =<< T.mapM (foldFixM f) e

mapFixM :: (Monad m, T.Traversable t, T.Traversable f) =>
  (f b -> m b) -> t (Fix f) -> m (t b)
mapFixM = T.mapM . foldFixM

-- }}}

-- Trans(F) / Dual(F) {{{

class Trans t u where
  trans :: t -> u

instance (Ana t f, Cata f u) => Trans t u where
  trans  = cata . ana

class TransF f g where
  transF :: Fix f -> Fix g

instance (Cata f t, Ana t g) => TransF f g where
  transF = ana . cata

class (Trans t u, Trans u t) => Dual t u where
  dual :: t -> u
  dual  = trans

instance (Dual t u) => Dual u t

class (TransF f g, TransF g f) => DualF f g where
  dualF :: Fix f -> Fix g
  dualF = transF

instance (DualF f g) => DualF g f

-- }}}

-- Expr(F) {{{

data Expr
  = I Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq)

instance Show Expr where
  show = \case
    I i       -> "(I " ++ show i ++ ")"
    Add e1 e2 -> "(Add " ++ show e1 ++ " " ++ show e2 ++ ")"
    Mul e1 e2 -> "(Mul " ++ show e1 ++ " " ++ show e2 ++ ")"

data ExprF a
  = IF Int
  | AddF a a
  | MulF a a
  deriving (Eq,Functor,F.Foldable,T.Traversable)

instance (Show a) => Show (ExprF a) where
  show = \case
    IF i       -> "(IF " ++ show i ++ ")"                      
    AddF e1 e2 -> "(AddF " ++ show e1 ++ " " ++ show e2 ++ ")" 
    MulF e1 e2 -> "(MulF " ++ show e1 ++ " " ++ show e2 ++ ")" 

instance Ana Expr ExprF where
  anaF = \case
    I i       -> IF i
    Add e1 e2 -> AddF (ana e1) (ana e2)
    Mul e1 e2 -> MulF (ana e1) (ana e2)

instance Cata ExprF Expr' where
  cataF = \case
    IF i       -> I' i
    AddF e1 e2 -> Add' (cata e1) (cata e2)
    MulF e1 e2 -> Mul' (cata e1) (cata e2)

-- }}}

-- Expr'(F) {{{

data Expr'
  = I' Int
  | Add' Expr' Expr'
  | Mul' Expr' Expr'
  deriving (Eq)

instance Show Expr' where
  show = \case
    I' i       -> "(I' " ++ show i ++ ")"
    Add' e1 e2 -> "(Add' " ++ show e1 ++ " " ++ show e2 ++ ")"
    Mul' e1 e2 -> "(Mul' " ++ show e1 ++ " " ++ show e2 ++ ")"

data Expr'F a
  = I'F Int
  | Add'F a a
  | Mul'F a a
  deriving (Eq,Functor,F.Foldable,T.Traversable)

instance (Show a) => Show (Expr'F a) where
  show = \case
    I'F i       -> "(I'F " ++ show i ++ ")"                      
    Add'F e1 e2 -> "(Add'F " ++ show e1 ++ " " ++ show e2 ++ ")" 
    Mul'F e1 e2 -> "(Mul'F " ++ show e1 ++ " " ++ show e2 ++ ")" 

instance Ana Expr' Expr'F where
  anaF = \case
    I' i       -> I'F i
    Add' e1 e2 -> Add'F (ana e1) (ana e2)
    Mul' e1 e2 -> Mul'F (ana e1) (ana e2)

instance Cata Expr'F Expr where
  cataF = \case
    I'F i       -> I i
    Add'F e1 e2 -> Add (cata e1) (cata e2)
    Mul'F e1 e2 -> Mul (cata e1) (cata e2)

-- }}}

nodes :: ExprF Int -> Int
nodes = \case
  IF i       -> 1
  AddF e1 e2 -> 1 + e1 + e2
  MulF e1 e2 -> 1 + e1 + e2

nodes' :: Expr'F Int -> Int
nodes' = \case
  I'F i       -> 1
  Add'F e1 e2 -> 1 + e1 + e2
  Mul'F e1 e2 -> 1 + e1 + e2

