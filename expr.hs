{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import Debug.Trace

-- Base {{{

data Expr f = In (f (Expr f))

infixr :+:

data (f :+: g) e = Inl (f e) | Inr (g e)
  deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f e = case e of
    Inl a -> Inl $ fmap f a
    Inr a -> Inr $ fmap f a

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)


-- }}}

-- Injection {{{

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

-- Reflexive case
instance Functor f => f :<: f where
  inj = id
  prj = Just

-- Base case, f is at head
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj cp = case cp of
    Inl e -> Just e
    Inr _ -> Nothing

-- Inductive case, f is somewhere in tail
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj cp = case cp of
    Inl _ -> Nothing
    Inr e -> prj e

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

-- }}}

-- Algebra {{{

data Val e = Val Int
  deriving (Eq,Show)
type IntExpr = Expr Val

data Add e = Add e e
  deriving (Eq,Show)
type AddExpr = Expr Add

instance Functor Val where
  fmap f (Val a) = Val a

instance Functor Add where
  fmap f (Add a b) = Add (f a) (f b)

data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

-- }}}

-- Eval {{{

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add e1 e2) = e1 + e2

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra cp = case cp of
    Inl x -> evalAlgebra x
    Inr y -> evalAlgebra y

eval :: Eval f => Expr f -> Int
eval e = foldExpr evalAlgebra e

-- }}}

-- Operators {{{

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 `add`

add :: (Add :<: f) => Expr f -> Expr f -> Expr f
x `add` y = inject (Add x y)

infixl 7 `mul`

mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
x `mul` y = inject (Mul x y)

-- }}}

-- Render {{{

class Render f where
  render :: Render g => f (Expr g) -> String

instance (Render f, Render g) => Render (f :+: g) where
  render cp = case cp of
    Inl x -> render x
    Inr y -> render y

instance Render Val where
  render (Val x) = show x

instance Render Add where
  render (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"

instance Render f => Show (Expr f) where
  show (In t) = render t

-- }}}

-- Monads {{{

data Term f a
  = Pure a
  | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f t = case t of
    Pure a   -> Pure $ f a
    Impure t -> Impure $ fmap (fmap f) t

instance Functor f => Monad (Term f) where
  return a = Pure a
  m >>= f  = case m of
    Pure a   -> f a
    Impure t -> Impure $ fmap (>>= f) t

injectM :: (g :<: f) => g (Term f a) -> Term f a
injectM = Impure . inj

data Zero a
type IdM = Term Zero

data One a = One
type OptionM = Term One

data Const e a = Const e
type ErrorM e = Term (Const e)

foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp t = case t of
  Pure x   -> pure x
  Impure x -> imp (fmap (foldTerm pure imp) x)

-- }}}

-- Calc {{{

data Incr t   = Incr Int t
data Recall t = Recall (Int -> t)
data Clear t  = Clear t

instance Functor Incr where
  fmap f (Incr i t) = Incr i (f t)

instance Functor Recall where
  fmap f (Recall g) = Recall (f . g)

instance Functor Clear where
  fmap f (Clear t) = Clear (f t)

incr :: (Incr :<: f) => Int -> Term f ()
incr i = injectM (Incr i (Pure ()))

recall :: (Recall :<: f) => Term f Int
recall = injectM (Recall Pure)

clear :: (Clear :<: f) => Term f ()
clear = injectM (Clear (Pure ()))

newtype Mem = Mem Int deriving Show

class Functor f => Run f where
  runAlgebra :: f (Mem -> (a,Mem)) -> (Mem -> (a,Mem))

instance Run Incr where
  runAlgebra (Incr k r) (Mem i) = r (Mem (i + k))

instance Run Recall where
  runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance Run Clear where
  runAlgebra (Clear r) (Mem i) = r (Mem 0)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra cp = case cp of
    Inl r -> runAlgebra r
    Inr r -> runAlgebra r

run :: Run f => Term f a -> Mem -> (a,Mem)
run = foldTerm (,) runAlgebra

tick :: Term (Recall :+: Clear :+: Incr) Int
tick = do
  y <- recall
  incr 1
  return y

-- }}}

-- IO {{{

getC :: (Teletype :<: f) => Term f Char
getC        = injectM (Get Pure)

putC :: (Teletype :<: f) => Char -> Term f ()
putC c      = injectM (Put c (Pure ()))

readF :: (FileSystem :<: f) => FilePath -> Term f String
readF fp    = injectM (Read fp Pure)

writeF :: (FileSystem :<: f) => FilePath -> String -> Term f ()
writeF fp s = injectM (Write fp s (Pure ()))

data Teletype a
  = Get (Char -> a)
  | Put Char a

instance Functor Teletype where
  fmap f cmd = case cmd of
    Get g   -> Get (f . g)
    Put c r -> Put c (f r)

data FileSystem a
  = Read FilePath (String -> a)
  | Write FilePath String a

instance Functor FileSystem where
  fmap f cmd = case cmd of
    Read fp g    -> Read fp (f . g)
    Write fp s r -> Write fp s (f r)

class Functor f => Exec f where
  execAlgebra :: f (IO a) -> IO a

instance Exec Teletype where
  execAlgebra cmd = case cmd of
    Get f   -> getChar >>= f
    Put c r -> putChar c >> r

instance Exec FileSystem where
  execAlgebra cmd = case cmd of
    Read fp f    -> readFile fp >>= f
    Write fp s r -> writeFile fp s >> r

instance (Exec f, Exec g) => Exec (f :+: g) where
  execAlgebra cp = case cp of
    Inl t -> execAlgebra t
    Inr t -> execAlgebra t

exec :: Exec f => Term f a -> IO a
exec = foldTerm return execAlgebra

cat :: FilePath -> Term (Teletype :+: FileSystem) ()
cat fp = do
  cs <- readF fp
  mapM putC cs
  return ()

-- }}}

addExample :: Expr (Val :+: Add)
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

x1 :: Expr (Add :+: Val)
x1 = val 30000 `add` val 1330 `add` val 7

x2 :: Expr (Val :+: Add :+: Mul)
x2 = val 80 `mul` val 5 `add` val 4

x3 :: Expr (Val :+: Mul)
x3 = val 6 `mul` val 7

x4 :: Expr (Val :+: Add :+: Mul)
x4 = val 3 `mul` (val 2 `add` val 4)

x5 :: Expr Val
x5 = val 3

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

dist :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
dist t = do
  Mul a b <- match t
  Add c d <- match b
  return (a `mul` c `add` a `mul` d)

showMaybe :: Render f => Maybe (Expr f) -> String
showMaybe = maybe "fail." show 

x :: Expr (Val :+: Add)
x = In y
y :: (Val :+: Add) (Expr (Val :+: Add))
y = Inl z
z :: Val (Expr (Val :+: Add))
z = Val 3

w :: ((Val :+: Add) :<: f) => Expr f -> Expr f
w a = a

