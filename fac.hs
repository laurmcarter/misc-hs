
data Term
  = Occ Var
  | Use Prim
  | Lit Integer
  | App Term Term
  | Abs Var Term
  | Rec Var Term

type Var = String
type Prim = String

data Value
  = Num Integer
  | Bool Bool
  | Fun (Value -> Value)

instance Show Value where
  show v = case v of
    Num i  -> show i
    Bool b -> show b
    Fun _  -> "<procedure>"

prjFun (Fun f) = f
prjFun x = error ("bad function: " ++ show x)
prjBool (Bool b) = b
prjBool x = error ("bad bool: " ++ show x)
prjNum (Num n) = n
prjNum x = error ("bad num: " ++ show x)

binop :: (a -> Value) -> (Integer -> Integer -> a) -> Value
binop inj f = Fun $ \i -> Fun $ \j -> inj $ f (prjNum i) (prjNum j)

type Env = [(Var,Value)]

getVal x env = case lookup x env of
  Just v  -> v
  Nothing -> error ("no value: " ++ show x)

eval :: Env -> Term -> Value
eval env t = case t of
  Occ x     -> getVal x env
  Use p     -> getVal p prims
  Lit k     -> Num k
  App t1 t2 -> prjFun (eval env t1) (eval env t2)
  Abs x t'  -> Fun $ \v -> eval ((x,v):env) t'
  Rec x m   -> f where f = eval ((x,f):env) m

times = binop Num (*)
minus = binop Num (-)
equal = binop Bool (==)
cond = Fun $ \b -> Fun $ \x -> Fun $ \y -> if prjBool b then x else y

prims = [("*",times),("-",minus),("==",equal),("if",cond)]

facTerm = Rec "f" $ Abs "n" $
  App (App (App (Use "if") (App (App (Use "==") (Occ "n")) (Lit 0))) (Lit 1))
      (App (App (Use "*") (Occ "n"))
           (App (Occ "f")
                (App (App (Use "-") (Occ "n")) (Lit 1))))

fac n = prjNum $ eval [] $ App facTerm $ Lit n

