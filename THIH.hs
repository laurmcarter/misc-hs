
module THIH where

import           Control.Monad
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid

-- Types {{{

type Id = String
enumId :: Int -> Id
enumId n = "v" ++ show n

data Kind
  = Star
  | Kfun Kind Kind
  deriving (Eq,Ord,Show)
infixr 4 `Kfun`

data Type
  = TVar Tyvar
  | TCon Tycon
  | TAp  Type Type
  | TGen Int
  deriving (Eq,Show)

data Tyvar = Tyvar
  { tvarId :: Id
  , tvarKind :: Kind
  } deriving (Eq,Ord,Show)

data Tycon = Tycon
  { tconId :: Id
  , tconKind :: Kind
  } deriving (Eq,Ord,Show)

tUnit    = TCon (Tycon "Unit" Star)
tChar    = TCon (Tycon "Char" Star)
tInt     = TCon (Tycon "Int" Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat   = TCon (Tycon "Float" Star)
tDouble  = TCon (Tycon "Double" Star)

tList    = TCon (Tycon "[]" (Star `Kfun` Star))
tArrow   = TCon (Tycon "(->)" (Star `Kfun` Star `Kfun` Star))
tTuple2  = TCon (Tycon "(,)" (Star `Kfun` Star `Kfun` Star))

tString  :: Type
tString  = list tChar

fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b
infixr 4 `fn`

list :: Type -> Type
list t = TAp tList t

pair :: Type -> Type -> Type
pair a b = TAp (TAp tTuple2 a) b

class HasKind t where
  kind :: t -> Kind
instance HasKind Tyvar where
  kind = tvarKind
instance HasKind Tycon where
  kind = tconKind
instance HasKind Type where
  kind t = case t of
    TCon tc -> kind tc
    TVar tv -> kind tv
    TAp t _ -> case kind t of
      Kfun _ k -> k

-- }}}

-- Unifying {{{

newtype Subst = Subst 
  { unSubst :: M.Map Tyvar Type }
  deriving (Eq,Show)

instance Monoid Subst where
  mempty = Subst M.empty
  mappend s1 s2 = Subst (unSubst s1 `M.union` unSubst s2)

nullSubst :: Subst
nullSubst = mempty

(+->) :: Tyvar -> Type -> Subst
u +-> t = Subst (M.singleton u t)

inSubst :: Tyvar -> Subst -> Maybe Type
inSubst u (Subst m) = M.lookup u m

toL :: Subst -> [(Tyvar,Type)]
toL = M.toList . unSubst

toSubst :: [(Tyvar,Type)] -> Subst
toSubst = Subst . M.fromList

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = mapSubst (apply s1) s2 <> s1
infixr 4 @@

mapSubst :: (Type -> Type) -> Subst -> Subst
mapSubst f s = Subst $ M.map f $ unSubst s

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (Subst (unSubst s1 `M.union` unSubst s2)) else fail "merge fails"
  where
  agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
              (M.keys (unSubst s1) `L.intersect` M.keys (unSubst s2))


class Types t where
  apply :: Subst -> t -> t
  tv    :: t -> [Tyvar]

instance Types Type where
  apply s t = case t of
    TVar u -> case inSubst u s of
      Just t -> t
      Nothing -> TVar u
    TAp l r -> TAp (apply s l) (apply s r)
    _ -> t
  tv t = case t of
    TVar u  -> [u]
    TAp l r -> tv l `L.union` tv r
    _       -> []

instance (Types a) => Types [a] where
  apply s = map (apply s)
  tv = L.nub . concat . map tv
mgu :: Monad m => Type -> Type -> m Subst
mgu t1 t2 = case (t1,t2) of
  (TAp l r,TAp l' r') -> do s1 <- mgu l l'
                            s2 <- mgu (apply s1 r) (apply s1 r)
                            return (s2 @@ s1)
  (TVar u,_)          -> varBind u t2
  (_,TVar u)          -> varBind u t1
  (TCon tc1,TCon tc2) | tc1 == tc2 -> return nullSubst
  _                   -> fail "types do not unify"

varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `elem` tv t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst
match t1 t2 = case (t1,t2) of
  (TAp l r,TAp l' r') -> do sl <- match l l'
                            sr <- match r r'
                            merge sl sr
  (TVar u,_) | kind u == kind t2 -> return (u +-> t2)
  (TCon tc1,TCon tc2) -> return nullSubst
  _                   -> fail "types do not match"

-- }}}

-- Classes {{{

data Qual t = [Pred] :=> t
  deriving (Eq,Show)

data Pred = IsIn Id Type
  deriving (Eq,Show)

instance Types t => Types (Qual t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  tv (ps :=> t) = tv ps `L.union` tv t

instance Types Pred where
  apply s (IsIn i t) = IsIn i (apply s t)
  tv (IsIn i t) = tv t

mguPred :: Pred -> Pred -> Maybe Subst
mguPred = lift mgu
matchPred :: Pred -> Pred -> Maybe Subst
matchPred = lift match

lift :: Monad m => (Type -> Type -> m t) -> Pred -> Pred -> m t
lift m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = fail "classes differ"

type Class = ([Id],[Inst])
type Inst  = Qual Pred

data ClassEnv = ClassEnv
  { classes :: M.Map Id Class
  , defaults :: [Type]
  } deriving (Show)

super      :: ClassEnv -> Id -> [Id]
super ce i = case M.lookup i $ classes ce of
  Just (is,_) -> is
  Nothing     -> error "unknown class"

insts      :: ClassEnv -> Id -> [Inst]
insts ce i = case M.lookup i $ classes ce of
  Just (_,its) -> its
  Nothing      -> error "unknown class"

defined :: Id -> ClassEnv -> Bool
defined i ce = M.member i $ classes ce

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classes = M.insert i c $ classes ce }

initialEnv :: ClassEnv
initialEnv = ClassEnv M.empty [tInteger,tDouble]

type EnvTrans = ClassEnv -> Maybe ClassEnv

(<:>) :: EnvTrans -> EnvTrans -> EnvTrans
(<:>) = (>=>)

composeAll :: [EnvTrans] -> EnvTrans
composeAll = foldr (<:>) return

addClass :: Id -> [Id] -> EnvTrans
addClass i is ce
  | defined i ce                   = fail "class already defined"
  | any (not . flip defined ce) is = fail "superclass not defined"
  | otherwise                      = return (modify ce i (is,[]))

addPreludeClasses :: EnvTrans
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTrans
addCoreClasses = composeAll
  [ addClass "Eq"      []
  , addClass "Ord"     ["Eq"]
  , addClass "Show"    []
  , addClass "Read"    []
  , addClass "Bounded" []
  , addClass "Enum"    []
  , addClass "Functor" []
  , addClass "Monad"   []
  ]

addNumClasses :: EnvTrans
addNumClasses = composeAll
  [ addClass "Num"        ["Eq","Show"]
  , addClass "Real"       ["Num","Ord"]
  , addClass "Fractional" ["Num"]
  , addClass "Integral"   ["Real","Enum"]
  , addClass "RealFrac"   ["Real","Fractional"]
  , addClass "Floating"   ["Fractional"]
  , addClass "RealFloat"  ["Real","Floating"]
  ]

addInst :: [Pred] -> Pred -> EnvTrans
addInst ps p@(IsIn i _) ce
  | not (defined i ce) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instances"
  | otherwise          = return (modify ce i c)
  where
  its = insts ce i
  qs  = [ q | (_ :=> q) <- its ]
  c   = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = isJust $ mguPred p q

exampleInsts :: EnvTrans
exampleInsts = composeAll
  [ addPreludeClasses
  , addInst [] (IsIn "Ord" tUnit)
  , addInst [] (IsIn "Ord" tChar)
  , addInst [] (IsIn "Ord" tInt)
  , let a = (TVar (Tyvar "a" Star))
        b = (TVar (Tyvar "b" Star))
      in
      addInst [IsIn "Ord" a,IsIn "Ord" b]
        (IsIn "Ord" (pair a b))
  ]

bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t) = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

byInst :: ClassEnv -> Pred -> Maybe [Pred]
byInst ce p@(IsIn i t) = msum [ tryInst it | it <- insts ce i ]
  where
  tryInst (ps :=> h) = do u <- matchPred h p
                          return (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = scEntail ce ps p ||
  case byInst ce p of
    Nothing -> False
    Just qs -> all (entail ce ps) qs

inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where
  hnf t = case t of
    TVar _ -> True
    TCon _ -> False
    TAp t _ -> hnf t

toHnfs :: Monad m => ClassEnv -> [Pred] -> m [Pred]
toHnfs ce ps = do
  pss <- mapM (toHnf ce) ps
  return $ concat pss

toHnf :: Monad m => ClassEnv -> Pred -> m [Pred]
toHnf ce p
  | inHnf p   = return [p]
  | otherwise = case byInst ce p of
    Nothing -> fail "context reduction"
    Just ps -> toHnfs ce ps

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
  loop rs ps = case ps of
    []    -> rs
    p:ps' | entail ce (rs++ps') p -> loop rs ps'
          | otherwise             -> loop (p:rs) ps'

reduce :: Monad m => ClassEnv -> [Pred] -> m [Pred]
reduce ce ps = do
  qs <- toHnfs ce ps
  return (simplify ce qs)

scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

-- }}}

-- Scheme {{{

data Scheme = Forall [Kind] (Qual Type)
  deriving (Eq,Show)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall ks qt) = tv qt

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
  vs' = [ v | v <- tv qt, v `elem` vs ]
  ks  = map kind vs'
  s   = toSubst $ zip vs' (map TGen [0..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)

data Assump = Id :>: Scheme deriving (Show)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (i :>: sc) = tv sc

find :: Monad m => Id -> [Assump] -> m Scheme
find i as = case as of
  [] -> fail ("unbound identifier: " ++ i)
  (i' :>: sc):as' -> if i == i' then return sc else find i as

newtype TI a = TI { runTI :: Subst -> Int -> (Subst,Int,a) }

instance Monad TI where
  return a = TI $ \s i -> (s,i,a)
  m >>= f  = TI $ \s i -> let (s',i',a) = runTI m s i in
    runTI (f a) s' i'

evalTI :: TI a -> a
evalTI m = x
  where
  (s,n,x) = runTI m nullSubst 0

getSubst :: TI Subst
getSubst = TI $ \s i -> (s,i,s)

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

extSubst :: Subst -> TI ()
extSubst s' = TI $ \s i -> (s'@@s,i,())

newTVar :: Kind -> TI Type
newTVar k = TI $ \s i -> let v = Tyvar (enumId i) k in (s,i+1,TVar v)

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTVar ks
  return (inst ts qt)

class Instantiate t where
  inst :: [Type] -> t -> t
instance Instantiate Type where
  inst ts t = case t of
    TAp l r -> TAp (inst ts l) (inst ts r)
    TGen n  -> ts !! n
    _       -> t
instance (Instantiate a) => Instantiate [a] where
  inst ts = map (inst ts)
instance (Instantiate t) => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-- }}}

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred],t)

-- {{{

data Literal
  = LitInt  Integer
  | LitChar Char
  | LitRat  Rational
  | LitStr  String
  deriving (Show)

tiLit :: Literal -> TI ([Pred],Type)
tiLit l = case l of
  LitChar _ -> return ([],tChar)
  LitInt  _ -> do v <- newTVar Star
                  return ([IsIn "Num" v],v)
  LitStr  _ -> return ([],tString)
  LitRat  _ -> do v <- newTVar Star
                  return ([IsIn "Fractional" v],v)

data Pat
  = PVar Id
  | PWildCard
  | PAs Id Pat
  | PLit Literal
  | PNpk Id Integer
  | PCon Assump [Pat]
  deriving (Show)

tiPat :: Pat -> TI ([Pred],[Assump],Type)
tiPat p = case p of
  PVar i               -> do v <- newTVar Star
                             return ([],[i :>: toScheme v], v)
  PWildCard            -> do v <- newTVar Star
                             return ([],[],v)
  PAs i pat            -> do (ps,as,t) <- tiPat pat
                             return (ps,(i :>: toScheme t):as,t)
  PLit l               -> do (ps,t) <- tiLit l
                             return (ps,[],t)
  PNpk i k             -> do t <- newTVar Star
                             return ([IsIn "Integral" t],[i :>: toScheme t],t)
  PCon (i :>: sc) pats -> do (ps,as,ts) <- tiPats pats
                             t' <- newTVar Star
                             (qs :=> t) <- freshInst sc
                             unify t (foldr fn t' ts)
                             return (ps++qs,as,t')

tiPats :: [Pat] -> TI ([Pred],[Assump],[Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ ps' | (ps',_,_) <- psasts ] 
      as = concat [ as' | (_,as',_) <- psasts ] 
      ts = [ t | (_,_,t) <- psasts ] 
  return (ps,as,ts)

data Expr
  = Var Id
  | Lit Literal
  | Const Assump
  | Ap Expr Expr
  | Let BindGroup Expr
  deriving (Show)

tiExpr :: Infer Expr Type
tiExpr ce as e = case e of
  Var i            -> do sc <- find i as
                         (ps :=> t) <- freshInst sc
                         return (ps,t)
  Const (i :>: sc) -> do (ps :=> t) <- freshInst sc
                         return (ps,t)
  Lit l            -> tiLit l
  Ap f v           -> do (ps,tf) <- tiExpr ce as f
                         (qs,tv) <- tiExpr ce as v
                         t <- newTVar Star
                         unify (tv `fn` t) tf
                         return (ps++qs,t)
  Let bg e         -> do (ps,as') <- tiBindGroup ce as bg
                         (qs,t) <- tiExpr ce (as'++as) e
                         return (ps++qs,t)

-- }}}

type Alt = ([Pat],Expr)

tiAlt :: Infer Alt Type
tiAlt ce as (pats,e) = do
  (ps,as',ts) <- tiPats pats
  (qs,t) <- tiExpr ce (as'++as) e
  return (ps++qs,foldr fn t ts)

tiAlts :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
tiAlts ce as alts t = do
  psts <- mapM (tiAlt ce as) alts
  mapM (unify t) (map snd psts)
  return (concat (map fst psts))

split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
  -> m ([Pred],[Pred])
split ce fs gs ps = do
  ps' <- reduce ce ps
  let (ds,rs) = L.partition (all (`elem` fs) . tv) ps'
  rs' <- defaultedPreds ce (fs++gs) rs
  return (ds,rs L.\\ rs')

stringInc :: String -> String
stringInc x = show (read x + 1 :: Int)

type Ambiguity = (Tyvar,[Pred])
ambiguities :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities ce vs ps = [ (v,filter (elem v . tv) ps) | v <- tv ps L.\\ vs ]

numClasses :: [Id]
numClasses =
  [ "Num"
  , "Integral"
  , "Floating"
  , "Fractional"
  , "Real"
  , "RealFloat"
  , "RealFrac"
  ]

stdClasses :: [Id]
stdClasses =
  [ "Eq"
  , "Ord"
  , "Show"
  , "Read"
  , "Bounded"
  , "Enum"
  , "Ix"
  , "Functor"
  , "Monad"
  , "MonadPlus"
  ] ++ numClasses

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v,qs) =
  [ t' | let is = [ i | IsIn i t <- qs ]
             ts = [ t | IsIn i t <- qs ]
       , all ((TVar v) ==) ts
       , any (`elem` numClasses) is
       , all (`elem` stdClasses) is
       , t' <- defaults ce
       , all (entail ce []) [IsIn i t' | i <- is ]
  ]

withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
  -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = fail "cannot resolve ambiguity"
  | otherwise    = return (f vps (map head tss))
  where
  vps = ambiguities ce vs ps
  tss = map (candidates ce) vps

defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults $ \vps ts -> concat (map snd vps)

defaultSubst :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults $ \vps ts -> toSubst $ zip (map fst vps) ts

type Expl = (Id,Scheme,[Alt])

tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
tiExpl ce as (i,sc,alts) = do
  (qs :=> t) <- freshInst sc
  ps <- tiAlts ce as alts t
  s <- getSubst
  let qs' = apply s qs
      t'  = apply s t
      fs  = tv (apply s as)
      gs  = tv t' L.\\ fs
      sc' = quantify gs (qs' :=> t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds,rs) <- split ce fs gs ps'
  if sc /= sc'
  then fail "signature too general"
  else if not (null rs)
  then fail "context too weak"
  else return ds

type Impl = (Id,[Alt])

restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where
  simple (i,alts) = any (null . fst) alts

tiImpls :: Infer [Impl] [Assump]
tiImpls ce as bs = do
  ts <- mapM (const $ newTVar Star) bs
  let is    = map fst bs
      scs   = map toScheme ts
      as'   = zipWith (:>:) is scs ++ as
      altss = map snd bs
  pss <- sequence (zipWith (tiAlts ce as') altss ts)
  s <- getSubst
  let ps' = apply s (concat pss)
      ts' = apply s ts
      fs = tv (apply s as)
      vss = map tv ts'
      gs = foldr1 L.union vss L.\\ fs
  (ds,rs) <- split ce fs (foldr1 L.intersect vss) ps'
  if restricted bs
  then let gs' = gs L.\\ tv rs
           scs' = map (quantify gs' . ([]:=>)) ts'
    in return (ds++rs,zipWith (:>:) is scs')
  else let scs' = map (quantify gs . (rs:=>)) ts'
    in return (ds,zipWith (:>:) is scs')

type BindGroup = ([Expl],[[Impl]])

tiBindGroup :: Infer BindGroup [Assump]
tiBindGroup ce as (es,iss) = do
  let as' = [ v :>: sc | (v,sc,alts) <- es ]
  (ps,as'') <- tiSeq tiImpls ce (as'++as) iss
  qss <- mapM (tiExpl ce (as''++as'++as)) es
  return (ps++concat qss, as''++as')

tiSeq :: Infer bg [Assump] -> Infer [bg] [Assump]
tiSeq ti ce as bss = case bss of
  [] -> return ([],[])
  bs:bss' -> do (ps,as') <- ti ce as bs
                (qs,as'') <- tiSeq ti ce (as'++as) bss'
                return (ps++qs,as''++as')

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
tiProgram ce as bgs = evalTI $ do
  (ps,as') <- tiSeq tiBindGroup ce as bgs
  s <- getSubst
  rs <- reduce ce (apply s ps)
  s' <- defaultSubst ce [] rs
  return (apply (s'@@s) as')

