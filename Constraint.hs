{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Constraint where

import Prelude hiding (id,(.))
import qualified Prelude
import Control.Category
import GHC.Exts (Constraint)

data Dict p where
  Dict :: p => Dict p

newtype a :- b = Sub (a => Dict b)
infixr 4 :-

instance Category (:-) where
  id  = refl
  (.) = trans

refl :: a :- a
refl = Sub Dict

trans :: (b :- c) -> (a :- b) -> (a :- c)
trans f g = Sub $ Dict \\ f \\ g

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r
infixl 1 \\

(***) :: (a :- b) -> (c :- d) -> (a,c) :- (b,d)
f *** g = Sub $ Dict \\ f \\ g

weaken1 :: (a,b) :- a
weaken1 = Sub Dict

weaken2 :: (a,b) :- b
weaken2 = Sub Dict

contract :: a :- (a,a)
contract = Sub Dict

(&&&) :: (a :- b) -> (a :- c) -> a :- (b,c)
f &&& g = Sub $ Dict \\ f \\ g

top :: a :- ()
top = Sub Dict

type family Ex (a :: *) (c :: Constraint) :: Constraint
type instance Ex ()   c = ()
type instance Ex Bool c = c

falso :: (() ~ a) :- Ex a c
falso = Sub Dict

bottom :: (() ~ Bool) :- c
bottom = falso


