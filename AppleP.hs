
module AppleP where

import Control.Applicative
import Control.Arrow
import Data.Foldable (foldMap)
import Data.Maybe

type Index   = Integer
type Gen     = Integer

data Var a   = Ground a | Var Index deriving (Eq,Show)

type Subst a = [(Index, Var a)]

data Term a = Term
  { runTerm :: Gen -> TRes a
  }

data TRes a  = TRes
  { termGen   :: Gen
  , termSubst :: Subst a
  , termIndex :: Index
  } deriving (Eq,Show)

project :: TRes a -> Var a
project t@(TRes g s i) = maybe (Var i) f $ lookup i s
  where
  f v = case v of
    Var i'   -> project $ TRes g s i'
    Ground a -> Ground a

testTerm1 = TRes undefined [(0,Var 1),(1,Var 2),(2,Ground 3)] 0
testTerm2 = TRes undefined [(0,Var 1),(1,Var 2),(2,Ground 3)] 1
testTerm3 = TRes undefined [(0,Var 1),(1,Var 2),(2,Ground 3)] 2
testTerm4 = TRes undefined [] 3

semiPrune1 :: Subst a -> (Index,Var a) -> Subst a
semiPrune1 s p@(i,v) = case v of
  Var i'   -> case lookup i' s of
    Just v'@(Var _) -> semiPrune1 s (i,v')
    Just (Ground _) -> [p]
    Nothing -> [p]
  Ground a -> [p]

semiPrune :: Subst a -> Subst a
semiPrune s = foldMap (semiPrune1 s) s

instance Functor Var where
  fmap f v = case v of
    Var i    -> Var i
    Ground a -> Ground $ f a

instance Functor TRes where
  fmap f (TRes g s i) = TRes g s' i
    where
    s' = map (second $ fmap f) s

instance Functor Term where
  fmap f t = Term $ fmap f . runTerm t

instance Applicative Term where
  pure a = Term $ \g -> TRes (g+1) [(g,Ground a)] g
  tf <*> ta = Term $ \g ->
    let f  = runTerm tf g
        a  = runTerm ta $ termGen f
        g' = termGen a
      in
      TRes (g' + 1) ((g',undefined) : termSubst f ++ termSubst a) g'

