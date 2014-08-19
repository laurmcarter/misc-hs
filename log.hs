{-# LANGUAGE DeriveDataTypeable #-}

import Data.Data
import Data.Generics.Uniplate.Data

import Control.Monad

data Log
  = Var Char
  | Neg Log
  | Conj Log Log
  | Disj Log Log
  | Impl Log Log
  deriving (Eq,Show,Typeable,Data)

type Rewrite = Log -> Maybe Log

(+++) :: Rewrite -> Rewrite -> Rewrite
(f +++ g) x = f x `mplus` g x

------------
-- Rules

emptyR :: Rewrite
emptyR e = Nothing

demorgan :: Rewrite
demorgan e = case e of
  Neg (Conj e1 e2) -> Just $ Disj (Neg e1) (Neg e2)
  Neg (Disj e1 e2) -> Just $ Conj (Neg e1) (Neg e2)
  _                -> Nothing

dblNeg :: Rewrite
dblNeg e = case e of
  Neg (Neg e) -> Just $ e
  _           -> Nothing

elimReflex :: Rewrite
elimReflex e = case e of
  Conj e1 e2 | e1 == e2 -> Just $ e1
  Disj e1 e2 | e1 == e2 -> Just $ e1
  _                     -> Nothing

elimImpl :: Rewrite
elimImpl e = case e of
  Impl e1 e2 -> Just $ Disj (Neg e1) e2
  _          -> Nothing

composeAll :: [Rewrite] -> Rewrite
composeAll rs = case rs of
  []      -> emptyR
  (r:rs') -> r +++ composeAll rs'
  
reduce = rewrite $ composeAll
           [ demorgan
           , dblNeg
           , elimImpl
           , elimReflex
           ]

------------
-- Test

testE1 = Neg (Conj (Neg (Disj (Var 'a') (Neg (Var 'b')))) (Var 'c'))
testE2 = Impl (Neg testE1) (Conj (Var 'a') (Var 'a'))


