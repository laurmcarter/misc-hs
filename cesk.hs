
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Cont

import qualified Data.Map as M

data Expr
  = Int Integer
  | Bool Bool
  | Void
  | Nil

  | Var String
  | Lambda String Expr
  | App Expr Expr

  | If Expr Expr Expr
  | Let String Expr Expr
  | Set String Expr

  | Cons Expr Expr
  | Add Expr Expr
  | Eql Expr Expr
  | Begin Expr Expr
 -- | LetCC String Expr
 -- | Throw String Expr
  deriving (Eq,Show)

data Val
  = IntV Integer
  | BoolV Bool
  | VoidV
  | NilV
  | Proc String Expr Env
  | ConsV Val Val
  deriving (Eq,Show)

type Loc = Integer

type Env = M.Map String Loc
type Stack = M.Map Loc Val

type EvalM = ContT Val (StateT Stack (Reader Env))
type Eval = EvalM Val

eval :: Expr -> Val
eval = unReader . unState . unCont . valof
  where
  unReader = flip runReader M.empty
  unState  = flip evalStateT M.empty
  unCont   = flip runContT return

valof :: Expr -> Eval
valof exp = case exp of
  Int n          -> return $ IntV n
  Bool b         -> return $ BoolV b
  Void           -> return VoidV
  Nil            -> return NilV

  Var x          -> lookupVar x
  Lambda x body  -> proc x body

  App rator rand -> do f <- valof rator
                       v <- valof rand
                       applyProc f v

  If tst con alt -> do t <- valof tst
                       test t con alt
                 
  Add n1 n2      -> do x <- valof n1
                       add x n2

  Cons e1 e2     -> do v1 <- valof e1
                       v2 <- valof e2
                       return (ConsV v1 v2)
                 
  Let x e body   -> do v <- valof e
                       extendEnv x v $
                         valof body
                 
  Eql e1 e2      -> do x <- valof e1
                       y <- valof e2
                       eql x y

  Set x e        -> do v <- valof e
                       setBang x v

  Begin e1 e2    -> do v1 <- valof e1
                       valof e2

{-
getCC :: Eval
getCC = ContT $ \k -> k $ ContV (ContT . k)
-}

test :: Val -> Expr -> Expr -> Eval
test t conseq altern = case t of
  BoolV b -> if b then valof conseq else valof altern
  _ -> error ("Attempt to test non-boolean: " ++ show t)

eql :: Val -> Val -> Eval
eql x y = return $ BoolV $ case (x,y) of
  (IntV n1,IntV n2) -> n1 == n2
  (BoolV b1,BoolV b2) -> b1 == b2
  _ -> error ("Cannot test " ++ show x ++ " and " ++ show y ++ " for equality")

add :: Val -> Expr -> Eval
add x n2 = case x of
  IntV n1' -> do
    y <- valof n2
    case y of
      IntV n2' -> return $ IntV (n1'+n2')
      _ -> error ("Attempt to add non-number: " ++ show y)
  _ -> error ("Attempt to add non-number: " ++ show x)

proc :: String -> Expr -> Eval
proc x body = do
  env <- ask
  return $ Proc x body env

lookupVar :: String -> Eval
lookupVar = applyEnv >=> applyStack

applyEnv :: String -> EvalM Loc
applyEnv x = do
  env <- ask
  maybe (error ("Unbound variable: " ++ x)) return $ M.lookup x env

applyStack :: Loc -> Eval
applyStack loc = do
  stack <- get
  maybe (error ("Unbound location: " ++ show loc)) return $ M.lookup loc stack

setBang :: String -> Val -> Eval
setBang x v = do
  loc <- applyEnv x
  modifyStack loc v

modifyStack :: Loc -> Val -> Eval
modifyStack loc v = do
  modify $ M.alter (const $ Just v) loc
  return VoidV

newLoc :: EvalM Loc
newLoc = do
  stack <- get
  case M.maxViewWithKey stack of
    Nothing -> return 0
    Just ((n,_),_)   -> return (n+1)

extendEnv :: String -> Val -> Eval -> Eval
extendEnv x v m = do
  loc <- newLoc
  local (M.insert x loc) $ do
    modifyStack loc v
    m

applyProc :: Val -> Val -> Eval
applyProc f v = case f of
  Proc x body env -> extendEnv x v $ valof body
  _ -> error ("Attempt to apply non-procedure: " ++ show f ++ " to " ++ show v)

