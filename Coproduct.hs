
{-# LANGUAGE MultiParamTypeClasses
           , TypeOperators
           , FlexibleInstances
           , FlexibleContexts
           #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                    2009.04.19
-- |
-- Module      :  Control.Functor.Coproduct
-- Copyright   :  Copyright (c) 2007--2009 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs, TypeOperators)
--
-- This module provides an extensible union type for functors.
--
--     * Wouter Swierstra. 2008. /Data types a la carte/, Functional
--         Pearl. JFP 18: 423--436.
----------------------------------------------------------------

module Control.Functor.Coproduct
    (
    -- * Functor coproducts
      (:+:)(..)
    
    -- ** Coproduct smart constructors/destructors
    , (:<:)(..), asInjectedInto, asProjectionOf
    
    -- ** Fixed-point smart constructors/destructors
    , asFixOf, asUnfixOf
    ) where

import Control.Functor.Fixedpoint
import Control.Applicative ((<$>))
import Data.Foldable
import Data.Traversable

----------------------------------------------------------------
----------------------------------------------------------------
-- There's a bug in Haddock 2.0.0.0 where it will drop parentheses
-- around strict fields, functions as arguments, etc. This is
-- apparently fixed in version 2.2.0
--
-- <http://trac.haskell.org/haddock/ticket/44>

-- | This is for doing coproduct types a la Swierstra (2008). I'm
-- not sure if the strictness helps.

infixr 5 :+:
data (f :+: g) e = Inl !(f e) | Inr !(g e)
--    deriving Show

-- Don't print the injector tags that the derived Show would print
instance (Show (f e), Show (g e)) => Show ((f :+: g) e) where
    show (Inl x) = show x
    show (Inr x) = show x

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl x) = Inl (fmap f x)
    fmap f (Inr y) = Inr (fmap f y)

-- Minimal instance, we could pass all functions through
instance (Foldable a, Foldable b) => Foldable (a :+: b) where
    foldMap f (Inl t) = foldMap f t
    foldMap f (Inr t) = foldMap f t

-- Minimal instance, we could pass all functions through
instance (Traversable a, Traversable b) => Traversable (a :+: b) where
    sequenceA (Inl t) = Inl <$> sequenceA t
    sequenceA (Inr t) = Inr <$> sequenceA t
    
    -- | A 'Kleisli'-lifting of fmap onto any Applicative
    traverse f (Inl t) = Inl <$> traverse f t
    traverse f (Inr t) = Inr <$> traverse f t


----------------------------------------------------------------
-- | This class is for smart constructors that inject a simple
-- functor into the coproduct, and smart destructors for projecting
-- a simple functor our of a coproduct.

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a
    prj :: sup a -> Maybe (sub a)


instance (Functor f) => (:<:) f f where
    inj = id
    prj = Just

instance (Functor f, Functor g) => (:<:) f (f :+: g) where
    inj         = Inl
    prj (Inl x) = Just x
    prj (Inr _) = Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => (:<:) f (h :+: g) where
    inj         = Inr . inj
    prj (Inl _) = Nothing
    prj (Inr y) = prj y


-- | A type-passing variant of 'inj' which injects a simple functor
-- into a given coproduct. This is named for infix use, like
-- 'asTypeOf'.
asInjectedInto :: (f :<: g) => f a -> g b -> g a
asInjectedInto  = const . inj


-- | A type-passing variant of 'prj' which strips off the coproduct
-- if the underlying functor matches a given functor. This is named
-- for infix use, like 'asTypeOf'.
asProjectionOf :: (f :<: g) => g a -> f b -> Maybe (f a)
asProjectionOf  = const . prj


----------------------------------------------------------------
----------------------------------------------------------------

-- | Inject a semi-fixed term into the coproduct of another given
-- term. This is most helpful when you don't know the coproduct
-- that the initial term recurses as (e.g. because you constructed
-- it with 'term'). This is named for infix use, like 'asTypeOf'.
asFixOf :: (f :<: g) => f (Fix g) -> Fix g -> Fix g
asFixOf  = const . Fix . inj
      -- == Fix ... asInjectedInto


-- | Take a fixed term, unfix the outermost iteration of the fixed
-- point, and then project out the underlying functor if it matches
-- another given semi-fixed term. This is named for infix use, like
-- 'asTypeOf'.
asUnfixOf :: (f :<: g) => Fix g -> f (Fix g) -> Maybe (f (Fix g))
asUnfixOf  = const . prj . unFix
        -- == asProjectionOf . unFix

----------------------------------------------------------------
----------------------------------------------------------- fin.
