{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TTypeable where

import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

data TypeRep a
  = TCon a [TypeRep a]

type family TypeCode (a :: *)  :: N
type family TypeOf   (ty :: *) :: TypeRep *
data family Rep      (ty :: *) :: *

data Any
data Any2 a

data RepAny
type instance TypeOf  Any       = TCon RepAny '[]
type instance TypeOf (Any2 a)   = TCon RepAny '[]
type instance TypeCode RepAny   = Z

data RepUnit
type instance TypeOf ()         = TCon RepUnit '[]
type instance TypeCode RepUnit  = S (TypeCode RepAny)

data RepBool
type instance TypeOf Bool       = TCon RepBool '[]
type instance TypeCode RepBool  = S (TypeCode RepUnit)

data RepChar
type instance TypeOf Char       = TCon RepChar '[]
type instance TypeCode RepChar  = S (TypeCode RepBool)

data RepInt
type instance TypeOf Int        = TCon RepInt  '[]
type instance TypeCode RepInt   = S (TypeCode RepChar)

data RepList
type instance TypeOf [a]        = TCon RepList '[TypeOf a]
type instance TypeCode RepList  = S (TypeCode RepInt)

data RepArrow
type instance TypeOf (a -> b)   = TCon RepArrow '[TypeOf a,TypeOf b]
type instance TypeCode RepArrow = S (TypeCode RepList)

data RepMaybe
type instance TypeOf (Maybe a)  = TCon RepMaybe '[TypeOf a]
type instance TypeCode RepMaybe = S (TypeCode RepArrow)

data RepIO
type instance TypeOf (IO a)     = TCon RepIO '[TypeOf a]
type instance TypeCode RepIO    = S (TypeCode RepMaybe)

{-
type family TypeOf ty :: *

type family TC_code tycon :: *

data ANY
data ANY2 a

data TRN_any
type instance TC_code TRN_any = Z       -- distinguished for TRN_any
type instance TypeOf ANY = (TRN_any, '[])
type instance TypeOf (ANY2 a) = (TRN_any, '[])
-}

-- Type-level booleans


-- Type-level natural numbers

data N
  = Z
  | S N

type instance x == y = NatEq x y
type family NatEq (x :: N) (y :: N) :: Bool where
  NatEq Z      Z    = True
  NatEq Z     (S y) = False
  NatEq (S x)  Z    = False
  NatEq (S x) (S y) = NatEq x y

-- Type-level lists


-- Comparison predicate on TYPEREP and its parts

type instance x == y = TypeRepEq x y
type family TypeRepEq (x :: TypeRep *) (y :: TypeRep *) :: Bool where
  TypeRepEq (TCon c1 t1) (TCon c2 t2) = (c1 == c2) && (t1 == t2)

type family (x :: TypeRep *) =*= (y :: TypeRep *) :: Bool where
  TCon c1 t1 =*= TCon c2 t2 = TypeRepEqN (TypeCode c1) (TypeCode c2) t1 t2
infix 4 =*=

type family TypeRepEqN (c1 :: N) (c2 :: N) (t1 :: [TypeRep *]) (t2 :: [TypeRep *]) :: Bool where
  TypeRepEqN  Z      c2    t1 t2 = True
  TypeRepEqN (S c1)  Z     t1 t2 = True
  TypeRepEqN (S c1) (S c2) t1 t2 = c1 == c2 && t1 =**= t2

type family (t1 :: [TypeRep *]) =**= (t2 :: [TypeRep *]) :: Bool where
  '[]       =**= '[]       = True
  '[]       =**= (b ': t2) = False
  (a ': t1) =**= '[]       = False
  (a ': t1) =**= (b ': t2) = a =*= b && t1 =**= t2
infix 4 =**=

type family Apply (lab :: *) (arg :: k) :: l

data AC_TypeOf
data AC_TypeRepEq
data AC_TypeRepEqW
data AC_MemberF
data Clos (f :: *) (x :: k)

type instance Apply AC_TypeOf           x  = TypeOf x
type instance Apply AC_TypeRepEq    '(x,y) = x == y
type instance Apply AC_TypeRepEqW   '(x,y) = x =*= y
type instance Apply AC_MemberF    '(x,y,z) = MemberF x y z
type instance Apply (Clos f x)          () = Apply f x

type family MemberF (f :: *) (x :: TypeRep *) (xs :: [TypeRep *]) :: Bool where
  MemberF f x '[]      = False
  MemberF f x (h ': t) = OrElse (Apply f '(x,h)) (Clos AC_MemberF '(f,x,t))

type Member  x xs = MemberF AC_TypeRepEq  x xs
type Member' x xs = MemberF AC_TypeRepEqW x xs

type family OrElse (p :: Bool) (q :: *) :: Bool where
  OrElse True  x = True
  OrElse False x = Apply x ()

ex1  = Proxy :: Proxy (TypeOf  (IO [Bool]))
ex2  = Proxy :: Proxy (TypeOf  (IO [Bool])   ==    TypeOf (IO [Bool]))
ex3  = Proxy :: Proxy (TypeOf  (IO [Bool])   ==    TypeOf (IO Bool))
ex4  = Proxy :: Proxy (TypeOf  (IO [Bool])   =*=   TypeOf (IO Any))
ex41 = Proxy :: Proxy (TypeOf  (IO [Any])    =*=   TypeOf (IO Bool))
ex42 = Proxy :: Proxy (TypeOf  (IO [Any])    =*=   TypeOf (IO [Int]))
ex5  = Proxy :: Proxy (Member  (TypeOf Bool)     '[TypeOf (),TypeOf Bool,TypeOf (IO ())])
ex6  = Proxy :: Proxy (Member  (TypeOf Int)      '[TypeOf (),TypeOf Bool,TypeOf (IO ())])
ex7  = Proxy :: Proxy (Member' (TypeOf (IO Any)) '[TypeOf Bool,TypeOf (IO ())])

type family ITE (t :: Bool) (c :: *) (a :: *) :: k where
  ITE True  c a = Apply c ()
  ITE False c a = Apply a ()

type family Map (f :: *) (as :: [k]) :: [l] where
  Map f '[]       = '[]
  Map f (a ': as) = Apply f a ': Map f as

