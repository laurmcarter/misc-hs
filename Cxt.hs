
module Cxt where

{-
succLemma :: Forall Nat (Exists Nat 
-}

{-

forall (x :: N). there exists a nat (y :: N). such that (y == S x)

T0 = Forall Nat (_A :: N -> *)
T1 = Exists Nat (_B :: N -> *) :: *


-}

{-
data Cxt r a where
  -- | needs a, results in r
  X     :: Cxt r r
  (:<:) :: Cxt (a -> r) (a -> r) -> a -> Cxt r (a -> r)
  (:>:) :: (a -> r) -> Cxt a a -> Cxt r a
infixl 5 :<:
infixr 4 :>:

fillCxt :: forall r a. Cxt r a -> a -> r
fillCxt c a = case c of
  X         -> a
  c' :<: x  -> fillCxt c' a x
  f  :>: c' -> f $ fillCxt c' a

c0 :: Cxt Bool Bool
c0 = not :>: X

c1 :: Cxt r (Bool -> r)
c1 = (.) X :<: True
-}

{-
appendNil :: List f as -> (as ++ bs) :~: '[] -> bs :~: '[]
appendNil as eq = case as of
  Nil      -> _
  _ :> as' -> _
-}

consApp :: p0 as -> p1 bs -> ((a ': as) ++ bs) :~: (a ': (as ++ bs))
consApp _ _ = Refl

data Cxt (r :: *) (as :: [*]) where
  X     :: Cxt x '[x]
  T     :: x -> Cxt x '[]
  (:@:) :: (ReifyFun as, ReifyFun bs) => Cxt (x -> y) as -> Cxt x bs -> Cxt y (as ++ bs)

runCxt :: forall r as. Cxt r as -> List Id as -> r
runCxt c = case c of
  X       -> \(Id a :> Nil) -> a
  T x     -> \Nil -> x
  (f :: Cxt (x -> r) bs) :@: (x :: Cxt x cs) -> \as  ->
    let g :: bs --> (x -> r)
        g = reflect $ runCxt f
        y :: cs --> x
        y = reflect $ runCxt x
    in undefined

class ReifyC as => ReifyFun (as :: [*]) where
  type ReifyC as :: Constraint
  reify   :: (as --> r) -> List Id as -> r
  reflect :: (List Id as -> r)                        -> (as --> r)
  hypo    :: p0 bs -> p1 r
          -> ((as ++ bs) --> r) -> List Id  as        -> (bs --> r)
  hyper   :: p0 as -> p1 r
          -> ( as        --> r) -> List Id (as ++ bs) -> (r,List Id bs)
  revert  :: C (ReifyC as)

instance ReifyFun '[] where
  type ReifyC '[] = ()
  reify        f Nil = f
  reflect      f     = f Nil
  hypo  bs_ r_ f Nil = f
  hyper   _  _       = (,)
  revert = C

instance ReifyC (a ': as) => ReifyFun (a ': as) where
  type ReifyC   (a ': as) = ReifyFun as
  reify        f (Id a :> as) = reify (f a) as
  reflect      f a            = reflect $ \as -> f $ Id a :> as
  hypo  bs_ r_ f (Id a :> as) = hypo bs_ r_ (f a) as
  hyper as_ (r_ :: p1 r) f (Id a :> (as :: List Id as')) = undefined
    where
    -- r :: List Id (as' ++ bs) -> (r,List Id bs)
    -- r = hyper (P :: P as') r_ (f a)
  hyper _ _ _ _ = error "unreachable"
  revert        = C

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:>) :: f a -> List f as -> List f (a ': as)

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

append :: List f as -> List f bs -> List f (as ++ bs)
append as bs = case as of
  Nil       -> bs
  fa :> as' -> fa :> append as' bs

