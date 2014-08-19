{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Families where

import Data.Proxy

type family Fst p where
  Fst (a,b) = a

type family Snd p where
  Snd (a,b) = b

-- Endpoints {{{

type family IxIn (m :: k -> k -> * -> *) (e :: *) :: Maybe k where
  IxIn m (m i j a) = Just i
  IxIn m (e1 -> e2) = IxIn m e1 :+: IxIn m e2
  IxIn m a = Nothing

type family IxOut (m :: k -> k -> * -> *) (e :: *) :: Maybe k where
  IxOut m (m i j a) = Just j
  IxOut m (e1 -> e2) = IxOut m e2 :+: IxOut m e1
  IxOut m a = Nothing

type family (mi :: Maybe k) :+: (mj :: Maybe k) :: Maybe k where
  Just a  :+: mj      = Just a
  Nothing :+: Just b  = Just b
  Nothing :+: Nothing = Nothing

ixIn :: Proxy m -> Proxy e -> Proxy (IxIn m e)
ixIn Proxy Proxy = Proxy

ixOut :: Proxy m -> Proxy e -> Proxy (IxOut m e)
ixOut Proxy Proxy = Proxy

-- }}}

-- Links {{{

type family Links (m :: k -> k -> * -> *) (e :: *) :: ([(k,k)],(k,k)) where
  Links m (e1 -> e2) =
        Links m e1
    :++ MakeLink (IxOut m e1) (IxIn m e2)
    :++ Links m e2
  Links m a = '[]

type family MakeLink (mi :: Maybe k) (mj :: Maybe k) :: [(k,k)] where
  MakeLink Nothing mj = '[]
  MakeLink mi Nothing = '[]
  MakeLink (Just i) (Just j) = '[ '(i,j) ]

type family (xs :: [k]) :++ (ys :: [k]) :: [k] where
  '[]       :++ '[]       = '[]
  '[]       :++ (y ': ys) = (y ': ys) :++ '[]
  (x ': xs) :++ ys        = x ': (xs :++ ys)

links :: Proxy m -> Proxy e -> Proxy (Links m e)
links Proxy Proxy = Proxy

type family p1 :|: p2 where
  (a,b) :|: (c,d) = (a,d)

-- }}}

-- Parallels {{{

type family Parallels (m :: k -> k -> * -> *) (e :: *) :: ([k],[k]) where
  Parallels m (m i j a)  = '( '[i] , '[j] )
  Parallels m (e1 -> e2) = Parallels m e1 :*: Parallels m e2
  Parallels m a          = '(  '[] ,  '[] )

type family p1 :*: p2 where
  '(a,b) :*: '(c,d) = '(a :++ c, b :++ d)

parallels :: Proxy m -> Proxy e -> Proxy (Parallels m e)
parallels Proxy Proxy = Proxy

-- }}}

type A = ()
type B = ()
type C = ()
type D = ()

data Ix =  I | J | K | L | M | N

data Mon (i :: Ix) (j :: Ix) a = Mon

fmapP :: Proxy ((A -> B) -> Mon I J A -> Mon K L B)
fmapP = Proxy

bindP :: Proxy (Mon I J A -> (A -> Mon K L B) -> Mon M N B)
bindP = Proxy

monP :: Proxy Mon
monP = Proxy

