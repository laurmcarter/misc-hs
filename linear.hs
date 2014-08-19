{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M
import Control.Lens hiding (Context (..))

type Name = String

data Kind
  = StarK
  | CircleK
  deriving (Eq,Show)

data Type
  = TVar TVar
  | Arrow Kind Type Type
  | Forall TVar Kind Type
  deriving (Eq,Show)

data TVar
  = BoundVar Name
  | FreeVar Integer
  deriving (Eq,Ord,Show)

data Var = V Name
  deriving (Eq,Ord,Show)

data Term
  = Var Var
  | Val Val
  | App Term Term
  | Inst Term Type
  deriving (Eq,Show)

data Val
  = Lam Kind (Var,Type) Term
  | Gen (TVar,Kind) Val
  deriving (Eq,Show)

-- Contexts {{{

newtype ValEnv = ValEnv
  { _valEnv :: M.Map Var Val
  } deriving (Eq,Show)

data Unrestricted = Unrestricted
  { _unrestrictedKindCxt :: M.Map TVar Kind
  , _unrestrictedTypeCxt :: M.Map TVar Type
  } deriving (Eq,Show)

newtype Restricted = Restricted
  { _restrictedTypeCxt :: M.Map TVar Type
  } deriving (Eq,Show)

data Context = Context
  { _valCxt          :: ValEnv
  , _unrestrictedCxt :: Unrestricted
  , _restrictedCxt   :: Restricted
  } deriving (Eq,Show)

makeFields ''ValEnv
makeFields ''Unrestricted
makeFields ''Restricted
makeLenses ''Context

emptyValEnv       = ValEnv       M.empty
emptyUnrestricted = Unrestricted M.empty M.empty
emptyRestricted   = Restricted   M.empty
emptyContext      = Context emptyValEnv emptyUnrestricted emptyRestricted

bindVal :: Var -> Val -> Context -> Context
bindVal x v = valCxt.env %~ M.insert x v

bindType :: Kind -> TVar -> Type -> Context -> Context
bindType StarK = bindUnrestrictedType
bindType CircleK = bindRestrictedType

bindRestrictedType :: TVar -> Type -> Context -> Context
bindRestrictedType tv t = restrictedCxt.typeCxt %~ M.insert tv t

bindUnrestrictedType :: TVar -> Type -> Context -> Context
bindUnrestrictedType tv t = unrestrictedCxt.typeCxt %~ M.insert tv t

bindUnrestrictedKind :: TVar -> Kind -> Context -> Context
bindUnrestrictedKind tv k = unrestrictedCxt.kindCxt %~ M.insert tv k

-- }}}

-- Evaluation {{{

runEval = eval emptyContext

eval :: Context -> Term -> Term
eval env expr = case expr of
  -- ???
  Inst (Val (Gen (tv,k) v)) t       -> eval (bindType k tv t env) $ Val v
  Inst e t                          -> eval env                   $ Inst (eval env e) t

  App (Val (Lam k (x,t) e)) (Val v) -> eval (bindVal x v env)       e
  App (Val v) e                     -> eval env                   $ App (Val v) (eval env e)
  App e1 e2                         -> eval env                   $ App (eval env e1) e2
  _                                 -> expr

-- }}}

