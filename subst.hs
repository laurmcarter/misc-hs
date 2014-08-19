{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M

data Expr a
  = V a
  | L a (Expr a)
  | A (Expr a) (Expr a)
  | I Int
  deriving (Eq,Show)

instance Functor Expr where
  fmap f = \case
    V x     -> V (f x)
    L x e   -> L (f x) (f <$> e)
    A e1 e2 -> A (f <$> e1) (f <$> e2)
    I i     -> I i

instance Monad Expr where
  return = V
  e >>= f = case e of
    V x     -> f x
    L x e   -> f x >>= \x' -> L x' (e >>= f)
    A e1 e2 -> A (e1 >>= f) (e2 >>= f)
    I i     -> I i

type E = Expr String
type Subst a = M.Map a a
type Eval = ReaderT (Subst String) Expr

