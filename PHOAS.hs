{-# LANGUAGE RankNTypes #-}

module PHOAS where

import Data.Profunctor

{-

data ExpF a
  = App a a
  | Lam (a -> a)

class Invariant f where
  invmap :: (a -> b) -> (b -> a) -> f a -> f b

instance Invariant ExpF where
  invmap f g e = case e of
    App e1 e2 -> App (f e1) (f e2)
    Lam fn    -> Lam (f . fn . g)

-}

{-

newtype Mu f = In { out :: f (Mu f) }

cata :: Invariant f => (f a -> a) -> (a -> f a) -> Mu f -> a
cata f g (In x) = f (invmap (cata f g) (ana f g) x)

ana :: Invariant f => (f a -> a) -> (a -> f a) -> a -> Mu f
ana f g x = In (invmap (ana f g) (cata f g) (g x))

-}

{-

data Rec f a = Place a | Roll (f (Rec f a))

cata :: Invariant f => (f a -> a) -> Rec f a -> a
cata f (Roll x) = f (invmap (cata f) Place x)
cata f (Place x) = x

type Exp = Rec ExpF

lam :: (Exp a -> Exp a) -> Exp a
lam f = Roll (Lam f)

app :: Exp a -> Exp a -> Exp a
app x y = Roll (App x y)

-}

{-

type Rec f a = (f a -> a) -> a

cata :: (f a -> a) -> Rec f a -> a
cata f x = x f

roll :: Invariant f => f (Rec f a) -> Rec f a
roll x f = f (invmap (cata f) place x)

place :: a -> Rec f a
place = const

type Exp a = Rec ExpF a

lam :: (Exp a -> Exp a) -> Exp a
lam f = roll (Lam f)

app :: Exp a -> Exp a -> Exp a
app x y = roll (App x y)

var :: a -> Exp a
var = place

type ClosedExp = forall x. Exp x

-}

{-

data Exp a
  = Var a
  | Lam (a -> Exp a)
  | App (Exp a) (Exp a)

-}

data ExpF a b
  = App b b
  | Lam (a -> b)

instance Profunctor ExpF where
  dimap f g (App x y) = App (g x) (g y)
  dimap f g (Lam h)   = Lam (g.h.f)

{-
data Rec p a b
  = Place b
  | Roll (p a (Rec p a b))
-}

type Exp = Rec ExpF

{-

instance Profunctor p => Monad (Rec p a) where
  return = Place
  Place b >>= f = f b
  Roll bs >>= f = Roll $ rmap (>>= f) bs

cata :: Profunctor p => (p a b -> b) -> Rec p a b -> b
cata phi (Place b)  = b
cata phi (Roll bs) = phi (rmap (cata phi) bs)

lam :: (a -> Exp a b) -> Exp a b
lam f = Roll (Lam f)

app :: Exp a b -> Exp a b -> Exp a b
app x y = Roll (App x y)

var :: b -> Exp a b
var = return

foo :: Exp a a
foo = lam $ \x -> lam $ \y -> app (var x) (var y)

type End p = forall x. p x x

iter0 :: Profunctor p => (p a a -> a) -> End (Rec p) -> a
iter0 phi x = cata phi x

-}

{-

newtype Rec p a b = Rec { runRec :: (p a b -> a) -> b }

instance Profunctor p => Profunctor (Rec p) where
  dimap f g (Rec h) = Rec $ \pab2a -> g $ h $ f . pab2a . dimap f g

-}

data Rec p a b = Rec 
  { runRec :: forall r.  (b -> r) -> (p a r -> r) -> r 
  }

instance Profunctor p => Profunctor (Rec p) where
  dimap f g m = Rec $ \kp kf -> runRec m (kp.g) (kf.lmap f)

instance Profunctor p => Monad (Rec p a) where
  return b = Rec $ \ br _ -> br b
  m >>= f  = Rec $ \kp kf -> 
    runRec m (\a -> runRec (f a) kp kf) kf

type End p = forall x. p x x

cata :: Profunctor p => (p a b -> b) -> Rec p a b -> b
cata phi m = runRec m id phi

roll :: Profunctor p => p a (Rec p a b) -> Rec p a b
roll w = Rec $ \kp kf -> kf (rmap (\r -> runRec r kp kf) w)

iter0 :: Profunctor p => (p a a -> a) -> End (Rec p) -> a
iter0 phi x = cata phi x

lam :: (a -> Exp a b) -> Exp a b
lam f = roll (Lam f)

app :: Exp a b -> Exp a b -> Exp a b
app x y = roll (App x y)

var :: b -> Exp a b
var = return

foo :: Exp a a
foo = lam $ \x -> lam $ \y -> app (var x) (var y)

