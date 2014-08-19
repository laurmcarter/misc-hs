{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

import Control.Arrow
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

type L u = LogicT (State (LState u))
type LState u = (Integer,Subst u)
type Subst u = M.Map Var (Term u)

initState :: LState u
initState = (0,M.empty)

data Term u
  = Var Var
  | Val u
  deriving (Eq,Show,Functor)

data x :+: y = Inl x | Inr y deriving (Eq,Show)
infixr 4 :+:

newtype Var = V { index :: Integer } deriving (Eq,Show)

-- (In|Pro)jection {{{

class Universe t u where
  inject  :: t -> Term u
  project :: Term u -> Maybe t

instance Universe Var u where
  inject = Var
  project t = case t of
    Var v -> Just v
    _     -> Nothing

instance Universe t t where
  inject = Val
  project t = case t of
    Val v -> Just v
    _     -> Nothing

instance Universe t (t :+: u) where
  inject = Val . Inl
  project t = case t of
    Val (Inl v) -> Just v
    _           -> Nothing

instance (Universe x y) => Universe x (z :+: y) where
  inject = fmap Inr . inject
  project t = case t of
    Val (Inr v) -> project (Val v)
    _           -> Nothing

-- }}}

fresh :: L u (Term u)
fresh = do
  (i,_) <- get
  modify (first (+1))
  return (Var (V i))

run :: Maybe Int -> L u a -> [a]
run mn = flip evalState initState . maybe observeAllT observeManyT mn



