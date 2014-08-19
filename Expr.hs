
import Prelude hiding (negate)

data Expr a
  = Var Char
  | Const a
  | (Expr a) :+: (Expr a)
  | (Expr a) :*: (Expr a)
  | (Expr a) :^: (Expr a)
  | (Expr a) :/: (Expr a)
  deriving (Show)

simplify :: (Num a, Eq a, Floating a) => Expr a -> Expr a
simplify e = case e of
  a :+: b -> let a' = simplify a
                 b' = simplify b
               in case (a',b') of
                 (Const c1, Const c2) -> Const (c1 + c2)
                 (_,Const 0) -> a'
                 (Const 0,_) -> b'
                 _ -> a' :+: b'

  a :*: b -> let a' = simplify a
                 b' = simplify b
               in case (a',b') of
                 (Const c1, Const c2) -> Const (c1 * c2)
                 (_,Const 1) -> a'
                 (Const 1,_) -> b'
                 (_,Const 0) -> Const 0
                 (Const 0,_) -> Const 0
                 (Const c1,Const c2 :*: e') -> Const (c1 * c2) :*: e'
                 (Const c1,e' :*: Const c2) -> Const (c1 * c2) :*: e'
                 (Const c1 :*: e',Const c2) -> Const (c1 * c2) :*: e'
                 (e' :*: Const c1,Const c2) -> Const (c1 * c2) :*: e'
                 (Const c1,a'' :+: b'') -> simplify (Const c1 :*: a'') :+: simplify (Const c1 :*: b'')
                 _ -> a' :*: b'

  a :^: b -> let a' = simplify a
                 b' = simplify b
               in case (a',b') of
                 (Const c1, Const c2) -> Const (c1 ** c2)
                 (_,Const 1) -> a'
                 (_,Const 0) -> Const 1
                 (c :^: Const b,Const a) -> c :^: (Const (a * b))
                 _ -> a' :^: b'

  a :/: b -> let a' = simplify a
                 b' = simplify b
               in case (a',b') of
                 (Const 0,_) -> Const 0
                 (_,Const 0) -> error "Division by zero"
                 (Const a,Const b) | a == b -> Const 1
                 (_,Const 1) -> a'
                 _ -> a' :/: b'
  _ -> e

negate :: (Num a) => Expr a -> Expr a
negate e = case e of
  Const c -> Const (-c)
  a :+: b -> negate a :+: negate b
  a :*: b -> negate a :*: b
  a :/: b -> negate a :/: b
  _ -> Const (-1) :*: e

mapVar :: (Char -> Expr a) -> Expr a -> Expr a
mapVar f e = case e of
  Var x   -> f x
  Const c -> Const c
  a :+: b -> mapVar f a :+: mapVar f b
  a :*: b -> mapVar f a :*: mapVar f b
  a :^: b -> mapVar f a :^: mapVar f b
  a :/: b -> mapVar f a :/: mapVar f b

plugIn :: Char -> a -> Expr a -> Expr a
plugIn x val = mapVar (\v -> if x == v then Const val else Var v)

derivative :: (Num a) => Expr a -> Expr a
derivative e = case e of
  Var x -> Const 1
  Const c -> Const 0
  a :+: b -> derivative a :+: derivative b
  a :*: b -> (a :*: derivative b) :+: (b :*: derivative a)
  a :^: Const x -> ((Const x) :*: (a :^: Const (x-1))) :*: derivative a
  a :/: b -> ((derivative a :*: b) :+: (negate (derivative b :*: a))) :/: (b :^: Const 2)
  _ -> error "no such rule"

ddx :: (Floating a, Eq a) => Expr a -> Expr a
ddx = simplify . derivative

ddxs :: (Floating a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Floating a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n e = ddxs e !! n

taylor :: (Floating a, Eq a) => Expr a -> [Expr a]
taylor e = fmap (simplify . series) es
  where
  indices = fmap fromIntegral [1..]
  derivs  = fmap (changeVars 'a') $ ddxs e
  changeVars c = mapVar $ const $ Var c
  facts   = fmap Const $ scanl1 (*) indices
  es   = zip (zipWith :/: derivs facts) indices
  series (expr,n) = expr :*: ((Var 'x' :+: (negate $ Var 'a')) :^: Const n)

