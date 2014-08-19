{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}

module SystemF where

import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S

-- Id {{{

newtype X = X String deriving (Eq,Show,Ord)
newtype A = A String deriving (Eq,Show,Ord)
type As   = S.Set A

class Named t where
  name :: t -> String

instance Named X where
  name (X x) = x

instance Named A where
  name (A a) = a

newtype Index = Index { index :: Integer } deriving (Eq,Show,Ord)

-- }}}

-- Env {{{

newtype Gamma = Gamma { unGamma :: M.Map X Type } deriving (Eq,Show)
newtype Delta = Delta { unDelta :: S.Set A } deriving (Eq,Show)

class FreeVars t where
  freeVars :: t -> As

instance FreeVars Type where
  freeVars (Forall as t) = freeVars t `S.difference` as

instance FreeVars BaseType where
  freeVars = \case
    VarT a      -> S.singleton a
    AtomT at    -> S.empty
    ArrowT t t' -> freeVars t `S.union` freeVars t'
    AppT t t'   -> freeVars t `S.union` freeVars t'
    TupleT ts   -> S.unions $ map freeVars ts

instance FreeVars Gamma where
  freeVars = S.unions . map freeVars . M.elems . unGamma

-- }}}

-- Expr {{{

data Term
  = Var X
  | TermAbs X Term
  | TermApp Term Term
  | Let X Term Term

  | If Term Term Term

  | Atom Atom
  | Op Prim Term Term

  | Tuple [Term]
  | Proj Index Term

  | Nil
  | Cons Term Term
  deriving (Eq,Show)

data Atom
  = I Integer
  | B Bool
  | C Char
  deriving (Eq,Show)

data Prim
  = Add
  | Sub
  | Mul
  deriving (Eq,Show)

-- }}}

-- Type {{{

data Type = Forall As BaseType
  deriving (Eq,Show)

data BaseType
  = VarT A
  | AtomT  AtomT
  | ArrowT BaseType BaseType
  | AppT   BaseType BaseType
  | TupleT [BaseType]
  deriving (Eq,Show)

data AtomT
  = IntT
  | BoolT
  | CharT
  deriving (Eq,Show)

-- }}}

newtype Subst t = Subst { unSubst :: M.Map A t } deriving (Eq,Show)

instance Monoid (Subst t) where
  mempty = Subst $ M.empty
  s1 `mappend` s2 = Subst (unSubst s1 <> unSubst s2) 

singleton :: A -> t -> Subst t
singleton a = Subst . M.singleton a

inSubst :: A -> Subst t -> Maybe t
inSubst a (Subst s) = M.lookup a s

remove :: X -> Gamma -> Gamma
remove x = Gamma . M.delete x . unGamma

generalize :: Gamma -> BaseType -> Type
generalize gamma tau = Forall as tau
  where
  as = freeVars tau `S.difference` freeVars gamma

substitute :: Subst BaseType -> BaseType -> BaseType
substitute s t = case t of
  VarT a      -> maybe t (substitute s) $ inSubst a s
  AtomT at    -> t
  ArrowT t t' -> ArrowT (substitute s t) (substitute s t')
  AppT   t t' -> AppT   (substitute s t) (substitute s t')
  TupleT ts   -> TupleT $ map (substitute s) ts

