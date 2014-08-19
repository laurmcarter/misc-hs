{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (sum, product)

import qualified Data.Foldable as F
import qualified Data.Traversable as T

data ExprF a
  = SumF a a
  | ProductF a a
  | ValF Double
  deriving (Show,Functor,F.Foldable,T.Traversable)

newtype Fix f = Fix { unFix :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  showsPrec n = showsPrec n . unFix

newtype Mu f = Mu { runMu :: forall a. (f a -> a) -> a }

fixToMu :: Functor f => Fix f -> Mu f
fixToMu (Fix expr) = Mu $ \f -> f . fmap  (($ f) . runMu . fixToMu) $ expr

type Sentence f = Fix f

type Model f a = f a -> a

type Expr = Sentence ExprF

runInterp :: Functor f => Model f a -> Sentence f -> a
runInterp i e = runMu (fixToMu e) i

interpExp :: Model ExprF Double
interpExp (SumF x y)     = x + y
interpExp (ProductF x y) = x * y
interpExp (ValF d)       = d

val :: Double -> Expr
val d = Fix $ ValF d

sum :: Expr -> Expr -> Expr
sum x y = Fix $ SumF x y

product :: Expr -> Expr -> Expr
product x y = Fix $ ProductF x y

instance Num Expr where
  (+) = sum
  (*) = product
  abs = undefined
  signum = undefined
  fromInteger = val . fromInteger

simpleExpr :: Expr
simpleExpr = 3 + 5

type CoModel f a = a -> f a
-- adjModel :: Model f a -> CoModel f a

