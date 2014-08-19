{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}

module Recover where

import Control.Applicative
import Data.List
import Data.Proxy
import GHC.Exts (Constraint)

data Env (env :: *) where
  Nil   :: Env ()
  (:>)  :: a -> Env env -> Env (a,env)
infixr 4 :>

-- Where we would otherwise have to define instance for each index...

{- 
instance Show (Env ()) where
  showsPrec d Nil = showString "Nil"

instance (Show a, Show (Env env)) => Show (Env (a,env)) where
  showsPrec d (a :> env) = showParen (d > 4)
      $ showsPrec 4 a
      . showString " :> "
      . showsPrec 4 env
-}

-- We can instead write...

instance Recover1 Show (Env env) => Show (Env env) where
  showsPrec d env = case env of
    Nil     -> showString "Nil"
    (a :: a) :> (e' :: Env env') ->
      recover1 (Dict :: Dict (Show env))
      // showParen (d > 4)
       $ showsPrec 4 a
       . showString " :> "
       . showsPrec 4 e'

-- Show' {{{

-- An alternate definition of the Show class
--  with instance super-constraints

class ShowC a => Show' (a :: *) where
  type ShowC a :: Constraint
  type ShowC a = ()
  ----
  showsPrec' :: Int -> a -> ShowS
  showsPrec' _ a = showString (show' a)
  show'      ::        a -> String
  show' a = showsPrec' 0 a ""
  showList' :: [a] -> ShowS
  showList' as = showChar '[' . go . showChar ']'
    where
    go = foldr (.) id
       $ intersperse (showChar ',')
       $ map shows' as

shows' :: Show' a => a -> ShowS
shows' = showsPrec' 0

-- instances 

instance Show' Bool where
  show' b = if b then "True" else "False"

instance Show' () where
  show' _ = "()"

instance Show' env => Show' (Env env) where
  type ShowC (Env env) = Show' env
  showsPrec' d e = case e of
    Nil     -> showString "Nil"
    a :> e' -> showParen (d > 4)
             $ showsPrec' 4 a
             . showString " :> "
             . showsPrec' 4 e'

instance (Show' a, Show' b) => Show' (a,b) where
  type ShowC (a,b) = (Show' a, Show' b)
  showsPrec' d (a,b) = showParen True
    $ showsPrec' 0 a
    . showChar ','
    . showsPrec' 0 b

instance Show' a => Show' [a] where
  type ShowC [a] = Show' a
  showsPrec' _ = showList'

-- }}}


data List (as :: [*]) where
  MT   :: List '[]
  (:*) :: a -> List as -> List (a ': as)
infixr 4 :*

instance Recover1 Show (List as) => Show (List as) where
  showsPrec d as = case as of
    MT -> showString "MT"
    (a :: a) :* (as' :: List as') ->
      recover1 (Dict :: Dict (Show (List as)))
      // showParen (d > 4)
       $ showsPrec 4 a
       . showString " :* "
       . showsPrec 4 as'

-- Recover1 {{{

class  R1 c a => Recover1 (c :: k -> Constraint) (a :: k) where
  type Rec1 c a :: Constraint
  type Rec1 c a = ()
  recover1 :: Dict (c a) -> Dict (Rec1 c a)
  recover1 Dict = Dict

type R1 c a = (c a, Rec1 c a)

instance Recover1 Show Double
instance Recover1 Show Bool
instance Recover1 Show Int
instance Recover1 Show ()

instance (R1 Show a, R1 Show b) => Recover1 Show (a,b) where
  type Rec1 Show (a,b) = (R1 Show a, R1 Show b)

instance R1 Show env => Recover1 Show (Env env) where
  type Rec1 Show (Env env) = R1 Show env

instance Recover1 Show (List '[])
instance R1 Show (List (a ': as)) => Recover1 Show (List (a ': as)) where
  type Rec1 Show (List (a ': as)) = (R1 Show a, R1 Show (List as))

-- }}}

-- Recover2 {{{

class  R2 c a b => Recover2 (c :: k -> l -> Constraint) (a :: k) (b :: l) where
  type Rec2 c a b :: Constraint
  type Rec2 c a b  = ()
  recover2 :: Dict (c a b) -> Dict (Rec2 c a b)
  recover2 Dict = Dict

type R2 c a b = (Rec2 c a b, c a b)

-- }}}

-- Dict {{{

data Dict (c :: Constraint) where
  Dict :: c => Dict c

deriving instance Show (Dict c)

(//) :: Dict c -> (c => r) -> r
Dict // r = r
infixr 0 //

(\\) :: (c => r) -> Dict c -> r
r \\ Dict = r
infixl 1 \\

-- }}}

