{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Interp where

import Unify.Fix

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Char (isAlpha,isDigit)

-- Type Definitions and Instances {{{

type Name = String

lexName :: ReadS String
lexName a =
  [ (x,b)
  | (x,b) <- lex a
  , all isValid x
  ]
  where
  isValid c = isAlpha c || isDigit c || c `elem` "-_"

data LC a
  = Var Name
  | Lam Name a
  | App a a
  deriving (Functor)
var :: (LC :<: f) => Name -> Mu f
var = inject . Var
lambda :: (LC :<: f) => Name -> Mu f -> Mu f
lambda = inject .: Lam
app :: (LC :<: f) => Mu f -> Mu f -> Mu f
app = inject .: App

instance Render LC where
  render lc = case lc of
    Var x          -> x
    Lam x body     -> "(lambda (" ++ x ++ ") " ++ show body ++ ")"
    App rator rand -> "(" ++ show rator ++ " " ++ show rand ++ ")"

instance Parse LC where
  parsesPrec _ a =
    [ (Var x,b)
    | (x,b) <- lexName a
    ] ++
    [ (Lam x body,h)
    | ("(",b)      <- lex a
    , ("lambda",c) <- lex b
    , ("(",d)      <- lex c
    , (x,e)        <- lexName d
    , (")",f)      <- lex e
    , (body,g)     <- reads f
    , (")",h)      <- lex g
    ] ++
    [ (App rator rand,e)
    | ("(",b)   <- lex a
    , (rator,c) <- reads b
    , (rand,d)  <- reads c
    , (")",e)   <- lex d
    ]

data Clos t a = Clos Name t (Env a) deriving (Functor)
clos :: (Clos t :<: f) => Name -> t -> Env (Mu f) -> Mu f
clos = inject .:: Clos

instance Render (Clos t) where
  render (Clos {}) = "<proc>"

data I a = I Integer deriving (Functor)
i :: (I :<: f) => Integer -> Mu f
i = inject . I

instance Render I where
  render (I x) = show x

instance Parse I where
  parsesPrec _ a =
    [ (I n,b)
    | (n,b) <- readsPrec (up_prec+1) a
    ]
    where
    up_prec = 10

data Add a = Add a a deriving (Functor)
add :: (I :<: f, Add :<: f) => Mu f -> Mu f -> Mu f
add = inject .: Add

instance Render Add where
  render (Add x y) = "(+ " ++ show x ++ " " ++ show y ++ ")"

instance Parse Add where
  parsesPrec _ a =
    [ (Add x y,f)
    | ("(",b) <- lex a
    , ("+",c) <- lex b
    , (x,d)   <- reads c
    , (y,e)   <- reads d
    , (")",f) <- lex e
    ]

data Mul a = Mul a a deriving (Functor)
mul :: (I :<: f, Mul :<: f) => Mu f -> Mu f -> Mu f
mul = inject .: Mul

instance Render Mul where
  render (Mul x y) = "(* " ++ show x ++ " " ++ show y ++ ")"

instance Parse Mul where
  parsesPrec _ a =
    [ (Mul x y,f)
    | ("(",b) <- lex a
    , ("*",c) <- lex b
    , (x,d)   <- reads c
    , (y,e)   <- reads d
    , (")",f) <- lex e
    ]

data SetBang a
  = SetBang Name a
  | Begin a a
  deriving (Functor)
setBang :: (SetBang :<: f) => Name -> Mu f -> Mu f
setBang = inject .: SetBang
begin  :: (SetBang :<: f) => Mu f -> Mu f -> Mu f
begin = inject .: Begin

instance Render SetBang where
  render sb = case sb of
    SetBang x e -> "(set! " ++ x ++ " " ++ show e ++ ")"
    Begin e1 e2 -> "(begin " ++ show e1 ++ " " ++ show e2 ++ ")"

instance Parse SetBang where
  parsesPrec _ a =
    [ (SetBang x exp,g)
    | ("(",b) <- lex a
    , ("set",c) <- lex b
    , ("!",d)   <- lex c
    , (x,e)   <- lex d
    , (exp,f) <- reads e
    , (")",g) <- lex f
    ] ++
    [ (Begin e1 e2,f)
    | ("(",b)     <- lex a
    , ("begin",c) <- lex b
    , (e1,d)      <- reads c
    , (e2,e)      <- reads d
    , (")",f)     <- lex e
    ]

data K a
  = LetCC Name a
  | Throw Name
  deriving (Functor)
letCC :: (K :<: f) => Name -> Mu f -> Mu f
letCC = inject .: LetCC
throw :: (K :<: f) => Name -> Mu f
throw = inject . Throw

instance Render K where
  render k = case k of
    LetCC x body -> "(letcc (" ++ x ++ ") " ++ show body ++ ")"
    Throw x      -> "(throw " ++ x ++ ")"

instance Parse K where
  parsesPrec _ a =
    [ (LetCC x body,h)
    | ("(",b)     <- lex a
    , ("letcc",c) <- lex b
    , ("(",d)     <- lex c
    , (x,e)       <- lexName d
    , (")",f)     <- lex e
    , (body,g)    <- reads f
    , (")",h)     <- lex g
    ] ++
    [ (Throw x,e)
    | ("(",b)     <- lex a
    , ("throw",c) <- lex b
    , (x,d)       <- lexName c
    , (")",e)     <- lex d
    ]

-- }}}

type Term1 = Mu LC
type Term2 = Mu (I :+: Add :+: Mul :+: LC)
type Term3 = Mu (I :+: Add :+: Mul :+: SetBang :+: K :+: LC)

type Val1 = Mu (Clos Term1)

type Env a = [(Name,a)]

eval1 :: Term1 -> Env Val1 -> Val1
eval1 exp env = case match exp of
  Just (Var x)          -> applyEnv env x
  Just (Lam x body)     -> clos x body env
  Just (App rator rand) -> applyProc1 (eval1 rator env) (eval1 rand env)
  _ -> error ("Unknown expression: " ++ show exp)

applyEnv :: Env a -> Name -> a
applyEnv env x = fromMaybe err $ lookup x env
  where
  err = error ("Unbound variable: " ++ x)

applyProc1 :: Val1 -> Val1 -> Val1
applyProc1 proc v = case match proc of
  Just (Clos x body env) -> eval1 body (extendEnv x v env)
  _ -> error ("Attempt to apply non-procedure: " ++ show proc)

extendEnv :: Name -> v -> Env v -> Env v
extendEnv x v = ((x,v):)

