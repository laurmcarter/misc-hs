{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import qualified Data.Map as Map

data Foo a = Foo
  { _bar  :: Int
  , _baz  :: Int
  , _quux :: a
  } deriving (Eq,Show)

makeLenses ''Foo

data Natural
  = Zero
  | Succ Natural
  deriving (Eq,Ord,Show)

--instance Show Natural where
--  show = show . toInteger

instance Real Natural where
  toRational n = case n of
    Zero    -> 0
    Succ n' -> 1 + toRational n'

instance Enum Natural where
  toEnum x = if x == 0
    then Zero
    else Succ (toEnum $ x - 1)
  fromEnum n = case n of
    Zero    -> 0
    Succ n' -> 1 + fromEnum n'

instance Num Natural where
  fromInteger x = if x == 0
    then Zero
    else Succ (fromInteger $ x - 1)
  n1 + n2 = case n1 of
    Zero     -> n2
    Succ n1' -> Succ (n1' + n2)
  n1 * n2 = case n1 of
    Zero     -> Zero
    Succ n1' -> n2 + (n1' * n2)
  abs = id
  signum n = case n of
    Zero   -> 0
    Succ _ -> 1
  n1 - n2 = case (n1,n2) of
    (_,Zero) -> n1
    (Succ n1',Succ n2') -> n1' - n2'
    _ -> error "No Such Thing as a Negative Natural"

instance Integral Natural where
  toInteger n = case n of
    Zero    -> 0
    Succ n' -> 1 + toInteger n'
  quotRem n1 n2 = if n1 > n2
    then let (q,r) = quotRem (n1 - n2) n2 in (Succ q,r)
    else if n1 == n2
    then (Succ Zero,Zero)
    else (Zero,n1)

nat :: SimplePrism Integer Natural
nat = prism toInteger $ \i ->
  if i < 0
  then Left i
  else Right $ fromInteger i

