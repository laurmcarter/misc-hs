{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module TDPE where

import Prelude hiding (Num(..))
import qualified Prelude as P
import Control.Lens

type RR r o1 o2 m1 m2 =
  Iso (r o1) (r o2) m1 m2

type RR' r o m = RR r o o m m

type AnRR r o1 o2 m1 m2 =
  AnIso (r o1) (r o2) m1 m2

type AnRR' r o m = AnRR r o o m m

base :: RR r a b (r a) (r b)
base = id

(-->) :: SymR r
  => AnRR r o11 o12 m11 m12
  -> AnRR r o21 o22 m21 m22
  -> RR r (o12 -> o21) (o11 -> o22) (m12 -> m21) (m11 -> m22)
(r1' :: AnRR r o11 o12 m11 m12)
  -->
  (r2' :: AnRR r o21 o22 m21 m22)
  = iso reflect_ reify_
  where
  reflect_ r = reflect r2 . app r . reify r1
  reify_   f = lam $ reify r2 . f . reflect r1
  ----
  r1 = cloneIso r1' :: RR r o11 o12 m11 m12
  r2 = cloneIso r2':: RR r o21 o22 m21 m22
infixr 8 -->

reify :: AnRR r o1 o2 m1 m2 -> m2 -> r o2
reify = view . coerced . from

reflect :: AnRR r o1 o2 m1 m2 -> r o1 -> m1
reflect = view . coerced . cloneIso

class SymR r where
  lam :: (r a -> r b) -> r (a -> b)
  app :: r (a -> b) -> r a -> r b
  int :: Int -> r Int
  (+) :: r Int -> r Int -> r Int

-- S {{{

newtype S a = S
 { sPrec :: Int -> Int -> ShowS
 }

display :: S a -> String
display s = sPrec s 0 0 ""

sConst :: String -> S a
sConst s = S $ \_ _ -> showString s

instance SymR S where
  lam f = S $ \c d -> let x = "x_" ++ show c
    in  showParen (d > 10)
      $ showString ("\\" ++ x ++ " -> ")
      . sPrec (f $ sConst x) (succ c) 0
  app f x = S $ \c d -> showParen (d > 10)
    $ sPrec f c 11
    . showString " "
    . sPrec x c 11
  int i = S $ \_ _ -> shows i
  x + y = S $ \c d -> showParen (d > 6)
    $ sPrec x c 7
    . showString " + "
    . sPrec y c 7

e0 :: SymR r => r ((Int -> Int) -> Int -> Int)
e0 = lam $ \f -> lam $ \x -> app f $ app f x

add :: SymR r => r Int -> r Int -> r Int
add = (+)

-- }}}

