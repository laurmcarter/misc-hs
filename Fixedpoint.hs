
-- This is for the Show (Fix f) instance
{-# LANGUAGE UndecidableInstances #-}

-- This is for 'build'
{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
-- To parse rules (not necessary in GHCi, but it is for compilation)
{-# OPTIONS_GHC -O2 -fglasgow-exts #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

----------------------------------------------------------------
--                                                    2009.07.10
-- |
-- Module      :  Control.Functor.Fixedpoint
-- Copyright   :  Copyright (c) 2007--2009 wren ng thornton
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (Rank2Types, ScopedTypeVariables)
--
-- This module provides a fixed point operator on functor types.
-- For Haskell the least and greatest fixed points coincide, so we
-- needn't distinguish them. This abstract nonsense is helpful in
-- conjunction with other category theoretic tricks like Swierstra's
-- coproduct types in "Control.Functor.Coproduct". For more on
-- the utility of two-level recursive types, see:
--
--     * Tim Sheard. 2001. /Generic Unification via Two-Level Types/
--         /and Paramterized Modules/. ICFP.
----------------------------------------------------------------

module Control.Functor.Fixedpoint
    (
    -- * Least fixed point operator for functors
      Fix(..)
    
    -- * Maps
    , ymap,  ymapM
    
    -- * Builders
    , build
    
    -- * Catamorphisms
    , cata,  cataM
    , ycata, ycataM
    
    -- * Anamorphisms
    , ana,   anaM
    
    -- * Hylomorphisms
    , hylo,  hyloM
    
    ) where

import Prelude          hiding (mapM, sequence)
import Control.Monad    hiding (mapM, sequence)
import Data.Traversable

----------------------------------------------------------------
----------------------------------------------------------------
-- | 'Fix' is the least fixed point operator on a 'Functor' type
-- @f@. This lets us invoke category theory to get recursive types
-- and operations over them without the type checker complaining
-- about infinite types.

newtype Fix f = Fix { unFix :: f (Fix f) }


-- This requires UndecidableInstances because the context is larger
-- than the head and so GHC can't guarantee that the instance safely
-- terminates. It is in fact safe however.
instance (Show (f (Fix f))) => Show (Fix f) where
    -- Don't show the wrappers, for legibility
    show (Fix f) = show f


----------------------------------------------------------------
-- | A version of 'fmap' for endomorphisms on the fixed point.
ymap :: (Functor f) => (Fix f -> Fix f) -> Fix f -> Fix f
ymap f = Fix . fmap f . unFix
{-# INLINE ymap #-}


-- | A monadic variant of 'ymap'.
ymapM :: (Traversable f, Monad m) => (Fix f -> m (Fix f)) -> Fix f -> m (Fix f)
ymapM f = liftM Fix . mapM f . unFix
{-# INLINE ymapM #-}


----------------------------------------------------------------
-- BUG: this isn't as helful as normal build/fold fusion as in Control.Functor.Fusable
--
-- | Take a church encoding of a fixed point into the data representation of the fixed point.
build :: (Functor f) => (forall r. (f r -> r) -> r) -> Fix f
build g = g Fix
{-# INLINE [1] build #-}

-- HACK: this requires -XScopedTypeVariables to avoid "Not in scope: type variable `f'". And it needs the type signature to ensure Rank-2 in r.
{-# RULES
"build/cata"  forall phi (g :: forall r. (f r -> r) -> r).
              cata phi (build g) = g phi
    #-}

----------------------------------------------------------------
-- | A pure catamorphism over the least fixed point, 'Fix', of a
-- Functor. This function applies an F-algebra from the bottom up
-- over @Fix f@ to create some residual value.
cata :: (Functor f) => (f a -> a) -> (Fix f -> a)
cata phi = self
    where
    self = phi . fmap self . unFix
{-# INLINE [0] cata #-}

-- We can't really use these because of the implication constraints
{- RULES
"cata-refl"  cata Fix = id

-- WTF: this seems like it should only work when @f@ is an endomorphism
-- (namely @id@). It'd almost work when @phi@ is rank-2 allomorphic
-- and @f@ preserves the allomorphism, except that the types of the
-- susequent wouldn't match.
"cata-fusion" forall f phi.
                  f . phi = phi . fmap f =>
                      f . cata phi = cata phi

"cata-compose" forall f g eps phi.
                   eps :: f :~> g =>
                       cata phi . cata (Fix . eps) = cata (phi . eps)
-}


-- | A catamorphism for monadic F-algebras. Alas, this isn't wholly
-- generic to @Functor@ since it requires distribution of @f@ over
-- @m@ (provided by 'Traversable').
cataM :: (Traversable f, Monad m) => (f a -> m a) -> (Fix f -> m a)
cataM phiM = self
    where
    self = phiM <=< (mapM self . unFix)
{-# INLINE cataM #-}


-- TODO: remove this, or add similar versions for ana* and hylo*?
-- | A variant of 'cata' which restricts the return type to being
-- a new Term. Though more restrictive, it can be helpful when you
-- already have an algebra like 'Dyna.Term.Variable.maybeVar'
-- which expects the outermost @Fix@.
--
-- If you don't like either @fmap@ or @cata@, then maybe this is
-- what you were thinking?
ycata :: (Functor f) => (Fix f -> Fix f) -> Fix f -> Fix f
ycata f = cata (f . Fix)
{-# INLINE ycata #-}


-- TODO: remove this, or add similar versions for ana* and hylo*?
-- | Monadic variant of 'ycata'.
ycataM :: (Traversable f, Monad m)
       => (Fix f -> m (Fix f)) -> Fix f -> m (Fix f)
ycataM f = cataM (f . Fix)
{-# INLINE ycataM #-}


----------------------------------------------------------------
-- | A pure anamorphism generating the greatest fixed point, 'Fix',
-- of a Functor. This function applies an F-coalgebra from the top
-- down to expand a seed into a @Fix f@.
ana :: (Functor f) => (a -> f a) -> (a -> Fix f)
ana psi = self
    where
    self = Fix . fmap self . psi
{-# INLINE ana #-}

-- We can't really use these because of the implication constraints
{- RULES
-- I think I dualized these all right...

"ana-refl"  ana unFix = id

"ana-fusion" forall f psi.
                  psi . f = fmap f . psi =>
                      ana psi . f = ana psi

"ana-compose" forall f g eps psi.
                   eps :: g :~> f =>
                       ana (eps . unFix) . ana psi = ana (eps . psi)
-}


-- | An anamorphism for monadic F-coalgebras. Alas, this isn't
-- wholly generic to @Functor@ since it requires distribution of
-- @f@ over @m@ (provided by 'Traversable').
anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> (a -> m (Fix f))
anaM psiM = self
    where
    self = (liftM Fix . mapM self) <=< psiM
{-# INLINE anaM #-}


----------------------------------------------------------------
-- Is this even worth mentioning? We can amortize the construction
-- of @Fix f@ (which we'd do anyways because of laziness), but we
-- can't fuse the @f@ away unless we inline all of @psi@, @fmap@,
-- and @phi@ at the use sites. Will inlining this definition be
-- sufficient to do that?

-- | @hylo phi psi == cata phi . ana psi@
hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> (a -> b)
hylo phi psi = self
    where
    self = phi . fmap self . psi
{-# INLINE hylo #-}


-- | @hyloM phiM psiM == cataM phiM <=< anaM psiM@
hyloM :: (Traversable f, Monad m)
      => (f b -> m b) -> (a -> m (f a)) -> (a -> m b)
hyloM phiM psiM = self
    where
    self = phiM <=< mapM self <=< psiM
{-# INLINE hyloM #-}

----------------------------------------------------------------
----------------------------------------------------------- fin.
