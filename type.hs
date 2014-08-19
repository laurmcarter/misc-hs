{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Data
import Data.Generics.Uniplate.Data

class VerifyScheme a b where
  vfs :: a -> b

instance VerifyScheme a b => VerifyScheme [a] [b] where
  vfs as = map vfs as

instance (VerifyScheme a c, VerifyScheme b d) => VerifyScheme (a,b) (c,d) where
  vfs (a,b) = (vfs a, vfs b)

instance VerifyScheme Tail1 Tail2 where
  vfs ta = case ta of
    AppT1        -> AppT2
    IfT1 p t1 t2 -> IfT2 (vfs p) (vfs t1) (vfs t2)
    BeginT1 es t -> BeginT2 (vfs es) (vfs t)

instance VerifyScheme Effect1 Effect2 where
  vfs ef = case ef of
    Set11 i      -> Set21 (-i)
    Set12        -> Set22
    IfE1 p e1 e2 -> IfE2 (vfs p) (vfs e1) (vfs e2)
    BeginE1 es e -> BeginE2 (vfs es) (vfs e)

instance VerifyScheme Pred1 Pred2 where
  vfs pr = case pr of
    AppP1         -> AppP2
    IfP1 p1 p2 p3 -> IfP2 (vfs p1) (vfs p2) (vfs p3)
    BeginP1 es p  -> BeginP2 (vfs es) (vfs p)

data Tail1
  = AppT1
  | IfT1 Pred1 Tail1 Tail1
  | BeginT1 [Effect1] Tail1
  deriving (Show,Eq)
data Effect1
  = Set11 Int
  | Set12
  | IfE1 Pred1 Effect1 Effect1
  | BeginE1 [Effect1] Effect1
  deriving (Show,Eq)
data Pred1
  = AppP1
  | IfP1 Pred1 Pred1 Pred1
  | BeginP1 [Effect1] Pred1
  deriving (Show,Eq)

data Tail2
  = AppT2
  | IfT2 Pred2 Tail2 Tail2
  | BeginT2 [Effect2] Tail2
  deriving (Show,Eq)
data Effect2
  = Set21 Int
  | Set22
  | IfE2 Pred2 Effect2 Effect2
  | BeginE2 [Effect2] Effect2
  deriving (Show,Eq)
data Pred2
  = AppP2
  | IfP2 Pred2 Pred2 Pred2
  | BeginP2 [Effect2] Pred2
  deriving (Show,Eq)

foo = BeginT1 [IfE1 AppP1 (Set11 5) Set12] $ IfT1 (IfP1 AppP1 (BeginP1 [] AppP1) AppP1) AppT1 AppT1
