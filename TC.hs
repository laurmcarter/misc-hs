{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import Unify

import Control.Monad.Reader
import Control.Monad.State.Class
import Data.Function (fix)
import Data.List
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type Id = String

data Exp
 = Var Id
 | Lam Id  Exp
 | App Exp Exp

 | Lit Lit
 | If  Exp Exp Exp
 | Eql Exp Exp
 
 | Let Id  Exp Exp
 | Fix Id  Exp

 | Add Exp Exp
 | Mul Exp Exp
 | Sub Exp Exp
 deriving (Eq,Show)

type EEnv = Id -> Val

data Lit
  = I Integer
  | B Bool
  | C Char
  deriving (Eq,Show)

data Val
  = LitV Lit
  | Clos (Val -> Val)

instance Show Val where
  showsPrec x v = showParen (x > 10) $
    case v of
      LitV l  -> showsPrec 11 l
      Clos f -> showString "<proc>"

-- TypeF {{{

data TypeF a
  = LitT   LitT
  | Arr    a a
  | VarT   Id
  | Forall Id a
  deriving (Eq,Show,Functor,F.Foldable,T.Traversable)

instance Equal TypeF where
  equal x y = case (x,y) of
    (LitT l1     ,LitT l2     ) -> l1 == l2
    (Arr t1 t2   ,Arr u1 u2   ) -> t1 == u1 && t2 == u2
    (VarT x1     ,VarT x2     ) -> x1 == x2
    (Forall x1 t1,Forall x2 t2) -> x1 == x2 && t1 == t2
    _                           -> False

instance Render TypeF where
  render tp = case tp of
    LitT l     -> "(LitT " ++ show l ++ ")"
    Arr t1 t2  -> "(Arr " ++ show t1 ++ " " ++ show t2 ++ ")"
    VarT x     -> "(VarT " ++ show x ++ ")"
    Forall x t -> "(Forall " ++ show x ++ " " ++ show t ++ ")"

instance Uni TypeF LV where
  uni tp1 tp2 = case (tp1,tp2) of
    (Arr t1 t2   ,Arr u1 u2   ) -> unify t1 u1 >> unify t2 u2
    (Forall x1 t1,Forall x2 t2) -> assert (x1 == x2) (show x1 ++ " and " ++ show x2 ++ " are not equal.") >> unify t1 t2
    _                           -> fail ("Couldn't unify " ++ render tp1 ++ " with " ++ render tp2)

litT :: (TypeF :<: f) => LitT -> Mu f
litT = inject . LitT

iT   :: (TypeF :<: f) => Mu f
iT   =  litT IT

bT   :: (TypeF :<: f) => Mu f
bT   =  litT BT

cT   :: (TypeF :<: f) => Mu f
cT   =  litT CT

arr    :: (TypeF :<: f) => Mu f -> Mu f -> Mu f
arr    =  inject .: Arr

varT   :: (TypeF :<: f) => Id -> Mu f
varT   =  inject . VarT

forall :: (TypeF :<: f) => Id -> Mu f -> Mu f
forall =  inject .: Forall

-- }}}

-- LV {{{

type Index = Integer

newtype LV a = LV Index deriving (Show,Functor,F.Foldable,T.Traversable)

instance Equal LV where
  equal (LV x) (LV y) = x == y

instance Render LV where
  render (LV x) = "_." ++ show x

instance Uni LV LV where
  occ (LV x) (LV y) = return (x == y)

instance LogicVar LV where
  data Package LV f = Package
    { index :: Index
    , substitution :: Subst (Mu f)
    } deriving (Show)
  initPkg = Package { index = 0, substitution = emptyS }
  varBehavior uvar v = extendS (inject uvar) v
  newVar = do
    x <- getGen
    modifyGen succ
    return $ LV x
  getSubst = gets substitution
  putSubst s' = modify $ \s -> s { substitution = s' }

getGen :: Unifiable f LV => GoalM LV f Index
getGen = gets index

modifyGen :: Unifiable f LV => (Index -> Index) -> Goal LV f
modifyGen f = modify $ \s -> s { index = f $ index s }

-- }}}

type TypeL = LP LV '[TypeF] '[]
type Type = Mu TypeF

data LitT = IT | BT | CT deriving (Eq,Show)

-- eval {{{

eval :: Exp -> EEnv -> Val
eval exp env = case exp of
 Var x      -> env x
 Lam x e    -> Clos (\v -> eval e (extendEEnv x v env))
 App f v    -> case eval f env of
                 Clos f' -> f' (eval v env)
                 x       -> error ("Attempt to apply non-procedure: " ++ show x)

 Lit l      -> LitV l
 If  t c a  -> case eval t env of
                 LitV (B b) -> if b then eval c env else eval a env
                 x          -> error ("Attempt to test non-bool: " ++ show x)
 Eql e e'   -> case (eval e env, eval e' env) of
                 (LitV x,LitV x') -> case (x,x') of
                   (I i,I i') -> LitV (B (i == i'))
                   (B b,B b') -> LitV (B (b == b'))
                   (C c,C c') -> LitV (B (c == c'))
                   (v,v') -> error ("Attempt to test equality on " ++ show v ++ " and " ++ show v')
                 (v,v') -> error ("Attempt to test equality on " ++ show v ++ " and " ++ show v')

 Let x e e' -> let v = eval e env in eval e' (extendEEnv x v env)
 Fix x e    -> fix (\v -> eval e (extendEEnv x v env))

 Add e e'   -> case (eval e env, eval e' env) of
                 (LitV (I x), LitV (I y)) -> LitV (I (x + y))
                 (x,y) -> error ("Attempt to add non-numbers: " ++ show x ++ " and " ++ show y)
 Mul e e'   -> case (eval e env, eval e' env) of
                 (LitV (I x), LitV (I y)) -> LitV (I (x * y))
                 (x,y) -> error ("Attempt to multiply non-numbers: " ++ show x ++ " and " ++ show y)
 Sub e e'   -> case (eval e env, eval e' env) of
                 (LitV (I x), LitV (I y)) -> LitV (I (x - y))
                 (x,y) -> error ("Attempt to subtract non-numbers: " ++ show x ++ " and " ++ show y)

emptyEEnv :: EEnv
emptyEEnv y = error ("Unbound variable: " ++ show y)

extendEEnv :: Id -> Val -> EEnv -> EEnv
extendEEnv x v env y = if x == y
  then v
  else env y

valof :: Exp -> Val
valof = flip eval emptyEEnv

-- }}}

type Gamma = [(Id,TypeL)]

type TC a = Gamma -> Exp -> TypeL -> GoalM LV (Base TypeL) a
type TypeRule = TC ()

typeIn :: Id -> Gamma -> GoalM LV (Base TypeL) TypeL
typeIn x gam = case lookup x gam of
  Just sig -> return sig
  Nothing   -> fail ("Unbound variable: " ++ show x)

extendGamma :: Id -> TypeL -> Gamma -> Gamma
extendGamma x v = ((x,v):)

typeCheck :: TypeRule
typeCheck gam x sig = disj
  [ tVar gam x sig
  , tLam gam x sig
  , tApp gam x sig
  , tLet gam x sig
  , tGen gam x sig
  ]

tVar :: TypeRule
tVar gam (Var x) sig = do
  sig' <- x `typeIn` gam
  sig === sig'
tVar _ x _ = fail ("Not a Var: " ++ show x)

tLam :: TypeRule
tLam gam (Lam x e) sig = fresh $ \tau tau' -> do
  sig === arr tau tau'
  typeCheck (extendGamma x tau gam) e tau'
tLam _ x _ = fail ("Not a Lam: " ++ show x)

tApp :: TypeRule
tApp gam (App e e') sig = fresh $ \tau tau' -> do
  sig === tau
  typeCheck gam e (arr tau' tau)
  typeCheck gam e' tau'
tApp _ x _ = fail ("Not an App: " ++ show x)

tLet :: TypeRule
tLet gam (Let x e e') sig = fresh $ \tau tau' -> do
  sig === tau
  typeCheck gam e tau'
  typeCheck (extendGamma x tau' gam) e' tau
tLet _ x _ = fail ("Not a Let: " ++ show x)

tGen :: TypeRule
tGen gam e sig = fresh $ \tau -> do
  let alpha = idFreeIn gam
  sig === forall alpha tau
  typeCheck gam e tau

special :: Gamma -> TypeL -> TypeL -> Goal LV (Base TypeL)
special gam t tau' = fresh $ \tau -> do
  let x = idFreeIn gam
  forall x tau === tau'

{-
tInst :: TypeRule
tInst gam e sig = 
-}

idFreeIn :: Gamma -> Id
idFreeIn gam = loop allIds
  where
  loop xs = let (x:xs') = xs in
    if x `elem` fvs
    then loop xs'
    else x
  fvs = freeIn gam

freeIn :: Gamma -> [Id]
freeIn = concatMap $ freeVars . snd

freeVars :: TypeL -> [Id]
freeVars = foldMu free

class Functor f => FreeVars f where
  free :: f [Id] -> [Id]

instance (FreeVars f, FreeVars g) => FreeVars (f :+: g) where
  free cp = case cp of
    Inl e -> free e
    Inr e -> free e

instance FreeVars TypeF where
  free tf = case tf of
    LitT l     -> []
    Arr t t'   -> t ++ t'
    VarT x     -> [x]
    Forall x t -> delete x t

instance FreeVars LV where
  free (LV x) = []
  
fac :: Exp
fac = Fix "f"
  (Lam "n"
    (If (Eql (Var "n")
             (Lit (I 0)))
        (Lit (I 1))
        (Mul (Var "n")
             (App (Var "f")
                  (Sub (Var "n")
                       (Lit (I 1)))))))

type1 :: Type
type1 = forall "a" $ forall "b" $ arr (varT "a") (varT "b")

type2 :: TypeL
type2 = forall "a" $ forall "b" $ arr (varT "a") (varT "b")

allIds :: [Id]
allIds = [ [c] | c <- allIds1 ] ++ [ c : rest | c <- allIds1, rest <- allIds ]
  where
  allIds1 = ['a'..'z']

