
import Control.Applicative
import Control.Monad.State

-- AST {{{1

data Module a = Module
  { declarations :: [TopStmt a]
  , main         :: [BlockStmt a]
  }

data TopStmt a
  = Import      Name               (Maybe [Name])   (Maybe Name)
  | TypeDef     Name               Type
  | TopTypeDecl Name               Type
  | TopLet      [(Name,Expr a)]

data BlockStmt a
  = Bind          (Maybe Name)     (Expr a)
  | BlockTypeDecl Name             Type
  | BlockLet      [(Name,Expr a)]

data Expr a
  -- Constants
  = Bit         Bool                          a
  | Quote       String                        a
  | Z           Integer                       a
  -- Structures
  | Array       [Expr a]                      a
  | Block       [BlockStmt a]                 a
  | Tuple       [Expr a]                      a
  | Record      [(Name, Expr a)]              a
  -- Accessors
  | Index       (Expr a)           (Expr a)   a
  | Lookup      (Expr a)           Name       a
  -- LC
  | Var         Name                          a
  | Function    Name a             (Expr a)   a
  | Application (Expr a)           [Expr a]   a
  -- Sugar
  | LetBlock    [(Name,Expr a)]    (Expr a)

data TypeF f
  -- Constants
  = Bit'
  | Z'
  | Quote'
  -- Structures
  | Array'       (Mu TypeF f)          Int
  | Block'       Context               (Mu TypeF f)
  | Tuple'       [Mu TypeF f]
  | Record'      [(Name,Mu TypeF f)]
  -- LC
  | Function'    (Mu TypeF f)          (Mu TypeF f)

instance Functor TypeF where
  fmap f t = case t of
    Bit'               -> Bit'
    Z'                 -> Z'
    Quote'             -> Quote'
    Array' t l         -> Array (f t) l
    Block' c t         -> Block c (f t)
    Tuple' ts          -> Tuple (map f ts)
    Record' fts        -> let (ns,ts) = unzip fts in
                            Record' (zip ns $ map f ts)
    Function' n argt t -> Function' n (f argt) (f t)

data Context

-- Aux Types {{{1

type Env = [(Name,Type)]

type TEnv = Env -- Map from Term (by name) to Type
type SEnv = Env -- Map from Type Synonym (by name) to Type

type Name = String

data Poly a
  = NoAnn
  | Annot a
  | Poly Int

instance Functor Poly where
  fmap f p = case p of
    NoAnn   -> NoAnn
    Annot a -> Annot $ f a
    Poly i  -> Poly i 

data Logic a
  = LVar Int
  | Term a

instance Functor Logic where
  fmap f l = case l of
    LVar i -> LVar i
    Term a -> Term $ f a

newtype Id a = Id a

instance Functor Id where
  fmap f (Id a) = Id $ f a

type Mu t f = f (t f)
type PType = Mu TypeF Poly
type LType = Mu TypeF Logic
type Type  = Mu TypeF Id

type PMod  = Module      PType
type PTStm = TopStmt     PType
type PBStm = BlockStmt   PType
type PExpr = Expr        PType

type LMod  = Module      LType
type LTStm = TopStmt     LType
type LBStm = BlockStmt   LType
type LExpr = Expr        LType

type TMod  = Module      Type
type TTStm = TopStmt     Type
type TBStm = BlockStmt   Type
type TExpr = Expr        Type

typeOf :: Expr a -> a
typeOf e = case e of
  Bit _ t           -> t
  Quote _ t         -> t
  Z _ t             -> t
  Array _ t         -> t
  Block _ t         -> t
  Tuple _ t         -> t
  Record _ t        -> t
  Index _ _ t       -> t
  Lookup _ _ t      -> t
  Var _ t           -> t
  Function _ _ t    -> t
  Application _ _ t -> t
  LetBlock _ e      -> typeOf e

type LS = State Int

instance Applicative State where
  pure = return
  mf <*> ma = do f <- mf
                 a <- ma
                 return $ f a

-- LVar Lifting {{{1

eval :: State Int a -> a
eval = flip evalState 0

liftMod :: PMod -> LMod
liftMod m = eval $ return Module <*> (mapM liftTStm $ declarations m) <*> (mapM liftBStm $ main m)

liftTStm :: PTStm -> LS LTStm
liftTStm s = case s of
  TopLet binds    -> pure TopLet <*> liftBinds binds
  Import n is n'  -> pure (Import n is n')
  TypeDef n t     -> pure (TypeDef n t)
  TopTypeDecl n t -> pure (TopTypeDecl n t)

liftBStm :: PBStm -> LS LBStm
liftBStm s = case s of
  Bind mn e         -> pure (Bind mn) <*> liftExpr e
  BlockLet binds    -> pure BlockLet <*> liftBinds binds
  BlockTypeDecl n t -> pure (BlockTypeDecl n t)

liftBinds :: [(Name,PExpr)] -> LS [(Name,LExpr)]
liftBinds = liftSnd liftExpr

liftArgs :: [(Name,PType)] -> LS [(Name,LType)]
liftArgs = liftSnd assignVar

liftExpr :: PExpr -> LS LExpr
liftExpr e = case e of
  Bit b t              -> pure (Bit b)   <*> assignVar t
  Quote s t            -> pure (Quote s) <*> assignVar t
  Z i t                -> pure (Z i)     <*> assignVar t

  Array es t           -> pure Array  <*> mapM liftExpr es <*> assignVar t
  Block ss t           -> pure Block  <*> mapM liftBStm
  Tuple es t           -> pure Tuple  <*> mapM liftExpr es <*> assignVar t
  Record fs t          -> pure Record <*> liftBinds fs     <*> assignVar t

  Index a i t          -> pure Index  <*> liftExpr a <*> liftExpr i <*> assignVar t
  Lookup r f t         -> pure Lookup <*> liftExpr r <*> liftExpr f <*> assignVar t

  Var n t              -> pure (Var n)     <*> assignVar t
  Function args body t -> pure Function    <*> liftArgs args <*> liftExpr body <*> assignVar t
  Application f v t    -> pure Application <*> liftExpr f    <*> liftExpr v    <*> assignVar t

  LetBlock binds body  -> pure LetBlock <*> liftBinds binds <*> liftExpr body

assignVar :: PType -> LType
assignVar t = do $
  n <- nextLVar
  case t of
    Poly i  -> 
    NoAnn   -> 
    Annot t -> fmap assignVar t

-- TC {{{1

newtype TC a = TC { runTC :: TEnv -> SEnv -> Either String (a,TEnv,SEnv) }

instance Functor TC where
  fmap f tc = TC $ \te se -> let m = runTC tc te se in
    case m of
      Left err            -> Left err
      Right (a, te', se') -> Right (f a, te', se')

instance Monad TC where
  return a = TC $ \te se -> Right (a,te,se)
  tc >>= f = TC $ \te se -> let m = runTC tc te se in
    case m of
      Left err          -> Left err
      Right (a,te',se') -> runTC (f a) te' se'

instance Applicative TC where
  pure = return
  tc1 <*> tc2 = do f <- tc1
                   a <- tc2
                   return $ f a

-- MHelpers {{{1

extendT :: Name -> Type -> TC ()
extendT n t = modifyT ((n,t):)

extendS :: Name -> Type -> TC ()
extendS n t = modifyS ((n,t):)

joinT :: TEnv -> TC ()
joinT te' = modifyT (te'++)

joinS :: SEnv -> TC ()
joinS se' = modifyS (se'++)

lookupT :: Name -> TC Type
lookupT n = do te <- getT
               case lookup n te of
                 Just t  -> return t
                 Nothing -> fail ("unbound variable: " ++ n)

lookupS :: Name -> TC Type
lookupS n = do se <- getS
               case lookup n se of
                 Just t  -> return t
                 Nothing -> fail ("unbound variable: " ++ n)

getT :: TC TEnv
getT = TC $ \te se -> Right (te,te,se)

getS :: TC SEnv
getS = TC $ \te se -> Right (se,te,se)

putT :: TEnv -> TC ()
putT te' = TC $ \te se -> Right ((),te',se)

putS :: SEnv -> TC ()
putS se' = TC $ \te se -> Right ((),te,se')

modifyT :: (TEnv -> TEnv) -> TC ()
modifyT f = do te <- getT
               putT (f te)

modifyS :: (SEnv -> SEnv) -> TC ()
modifyS f = do se <- getS
               putS (f se)

save :: TC a -> TC a
save m = do te <- getT
            se <- getS
            res <- m
            putT te
            putS se
            return res

liftSnd :: Monad m => (b -> c) -> [(a,b)] -> m [(a,c)]
liftSnd f xs = let (as,bs) = unzip xs in
  mapM f bs >>= return . zip as

-- TODO: Parameterize DoBlock blocks over monadic contexts and return type.
