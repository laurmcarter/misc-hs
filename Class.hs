{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Class where

import GHC.Exts (Constraint)
import GHC.TypeLits
import Control.Category
import Prelude hiding (id,(.))
import Data.Typeable

import qualified Data.Constraint as C
import Data.Constraint.All
import Data.Constraint.Member

-- Class {{{

class Class (c :: Symbol) where
  type Arity c    :: N
  type Sig   c (as :: [k]) :: [(Symbol,*)]
  type Super c (as :: [k]) :: Constraint
  recoverSuper :: Instance c as ==> Super c as
  default recoverSuper :: (Super c as ~ (() :: Constraint))
    => Instance c as ==> Super c as
  recoverSuper = termImpl

-- }}}

-- Instance {{{

type  c :@ as = (Class c, Length as ~ Arity c)
infix 4 :@

class (c :@ as) => Instance (c :: Symbol) (as :: [k]) where
  instDict :: Dict c as

-- }}}

-- Dict {{{

data Id (x :: Symbol) = Id

instance KnownSymbol x => Show (Id x) where
  showsPrec d x = showParen (d > 10)
    $ showString "Id "
    . shows (symbolVal x)

type (x :: Symbol) ::: (t :: *) = '(x,t)
infix 6 :::

data D (fs :: [(Symbol,*)]) where
  Nil  :: D '[]
  (:=) :: Id x -> t -> D fs -> D (x ::: t ': fs)
infix 1 :=

{-
instance Show (D '[]) where
  show Nil = "Nil"

instance (KnownSymbol x, Typeable t, Show (D fs))
  => Show (D (x ::: t ': fs)) where
  showsPrec d ((:=) x t fs) = showParen (d > 1)
    $ showChar '{'
    . shows x
    . showString " :: "
    . shows (typeOf t)
    . 
-}

data Dict (c :: Symbol) (as :: [k]) where
  Dict :: (c :@ as)
       => D (Sig c as) -> Dict c as

-- }}}

-- Proof (==>) (<==>) {{{

data Proof (c :: Constraint) where
  Proof :: c => Proof c

newtype (a :: Constraint) ==> (b :: Constraint) = Impl
  { impl :: a => Proof b
  }
infixr 5 ==>

idImpl :: a ==> a
idImpl = Impl Proof

transImpl :: b ==> c -> a ==> b -> a ==> c
transImpl bc ab = Impl $ Proof \\ impl bc \\ impl ab

termImpl :: a ==> (() :: Constraint)
termImpl = Impl Proof

instance Category (==>) where
  id  = idImpl
  (.) = transImpl

data (a :: Constraint) <==> (b :: Constraint) = Bicond
  { biTo   :: a ==> b
  , biFrom :: b ==> a
  }
infixr 5 <==>

idBicond :: a <==> a
idBicond = Bicond id id

transBicond :: b <==> c -> a <==> b -> a <==> c
transBicond bc ab = Bicond
  { biTo   = biTo   bc . biTo   ab
  , biFrom = biFrom ab . biFrom bc
  }

instance Category (<==>) where
  id  = idBicond
  (.) = transBicond

instance Groupoid (<==>) where
  inv ab = Bicond
    { biTo   = biFrom ab
    , biFrom = biTo   ab
    }

class Category cat => Groupoid (cat :: k -> k -> *) where
  inv :: cat a b -> cat b a

-- }}}

-- HasProof {{{

class HasProof (t :: *) where
  type ProofOf t :: Constraint
  type Residue t :: Constraint
  type Residue t = ()
  (//) :: Residue t => t -> (ProofOf t => r) -> r
  (\\) :: Residue t => (ProofOf t => r) -> t -> r
  infixr 0 //
  infixl 1 \\

instance HasProof (Proof c) where
  type ProofOf (Proof c) = c
  Proof // r = r
  r \\ Proof = r

instance HasProof (a ==> b) where
  type ProofOf (a ==> b) = b
  type Residue (a ==> b) = a
  Impl Proof // r = r
  r \\ Impl Proof = r

instance HasProof (a <==> b) where
  type ProofOf (a <==> b) = b
  type Residue (a <==> b) = a
  r \\ Bicond (Impl Proof) _ = r
  Bicond (Impl Proof) _ // r = r

-- }}}

-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

type family Length (as :: [k]) :: N where
  Length '[]       = Z
  Length (a ': as) = S (Length as)

type N0  = Z
type N1  = S N0
type N2  = S N1
type N3  = S N2
type N4  = S N3
type N5  = S N4
type N6  = S N5
type N7  = S N6
type N8  = S N7
type N9  = S N8
type N10 = S N9

-- }}}

-- Examples

instance Class "Eq" where
  type Arity "Eq"    = N1
  type Super "Eq" as = ()
  type Sig   "Eq" '[(a :: *)] =
    '[ "==" ::: (a -> a -> Bool)
     , "/=" ::: (a -> a -> Bool)
     ]

instance Instance "Eq" '[Bool] where
  instDict = eqDict

instance Instance "Eq" '[Int] where
  instDict = eqDict

eqDict :: Eq a => Dict "Eq" '[a]
eqDict = Dict $ (==) & (/=) & Nil

(&) :: t -> D fs -> D (x ::: t ': fs)
(&) = (Id :=)
infixr 1 &

