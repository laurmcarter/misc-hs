{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

import Prelude hiding (LT,GT,EQ)
import Control.Applicative
import Control.Arrow
import Control.Monad.Logic
import Control.Monad.State

type L = LogicT (State (Env T,Env S))
type Env a = [(Var,a)]
type Index = Int

newtype Var = Var
  { index :: Index
  } deriving (Eq,Ord,Show)

newtype Z = Z { unZ :: Integer } deriving (Eq,Ord,Show)

data Set
  = Empty
  | Ins T S
  | IntI Z Z S
  | IntS Set Set S
  deriving (Eq,Show)

data x :+: y = L x | R y deriving (Eq,Show)
infixr 4 :+:

type S = Var :+: Set
type T = Var :+: Z :+: Set

-- Projections {{{

class Has x y where
  inject  :: x -> y
  project :: y -> Maybe x

instance Has x x where
  inject = id
  project = Just

instance Has x (x :+: y) where
  inject = L
  project cp = case cp of
    L e -> Just e
    R _ -> Nothing

instance (Has x y) => Has x (z :+: y) where
  inject = R . inject
  project cp = case cp of
    L _ -> Nothing
    R e -> project e

var :: (Has Var x) => Index -> x
var  = inject . Var

empty :: (Has Set x) => x
empty  = inject Empty
ins   :: (Has Set x) => T -> S -> x
ins    = inject .: Ins
intI  :: (Has Set x) => Z -> Z -> S -> x
intI   = inject .:. IntI
intS  :: (Has Set x) => Set -> Set -> S -> x
intS   = inject .:. IntS

z :: (Has Z x) => Integer -> x
z  = inject . Z

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.:)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(op `on` f) x y = f x `op` f y

-- }}}

data Rel
  = EQ T T
  | IsSet T
  | IsInteger T
  | SRel SRel 
  | IRel IRel
  | Neg Rel
  deriving (Eq,Show)

data SRel
  = Member T T
  | Subset T T
  | Union T T T
  | Disj T T T
  | Diff T T T
  | Size T T
  deriving (Eq,Show)

data IRel
  = LE T T
  | LT T T
  deriving (Eq,Show)

-- Preds {{{

match :: (Has t u) => (t -> r) -> u -> Either r u
match f u = maybe (Right u) (Left . f) $ project u

boundedS' :: (Has Var u, Has Set u) => u -> Either Bool u
boundedS' = match boundedVar >=> match boundedSet

boundedT' :: (Has Z u, Has Var u, Has Set u) => u -> Either Bool u
boundedT' = match boundedZ >=> boundedS'

boundedS :: S -> Bool
boundedS = grndE . boundedS'
boundedT :: T -> Bool
boundedT = grndE . boundedT'

grndE :: Either Bool a -> Bool
grndE e = case e of
  Left b  -> b
  Right _ -> error "unhandled sum type"

closedS' :: (Has Var u, Has Set u) => u -> Either Bool u
closedS' = match closedVar >=> match closedSet

closedT' :: (Has Z u, Has Var u, Has Set u) => u -> Either Bool u
closedT' = match closedZ >=> closedS'

closedS :: S -> Bool
closedS = grndE . closedS'
closedT :: T -> Bool
closedT = grndE . closedT'

boundedZ (Z _) = True
boundedVar (Var _) = False
boundedSet s = case s of
  Empty      -> True
  Ins t s    -> boundedS s
  IntI m n s -> boundedS s
  IntS a b s -> boundedS s

closedZ (Z _) = True
closedVar (Var _) = False
closedSet s = case s of
  Empty      -> True
  Ins t s    -> and [closedT t,closedS s]
  IntI m n s -> closedS s
  IntS a b s -> and [closedSet a,closedSet b,closedS s]

-- }}}

