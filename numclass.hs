{-# LANGUAGE MultiWayIf #-}

data Exp
  = Num Integer
  | Neg Exp
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  deriving (Eq,Show)

instance Num Exp where
  fromInteger = Num
  negate = Neg
  (+) = Add
  (-) = Sub
  (*) = Mul
  signum n = if
    | i > 0 -> Num 1
    | i < 0 -> Num (-1)
    | True  -> Num 0
    where
    i = eval n
  abs n = if signum n == Num (-1)
    then Neg n
    else n

eval :: Exp -> Integer
eval n = case n of
  Num i   -> i
  Neg n   -> negate $ eval n
  Add x y -> eval x + eval y
  Sub x y -> eval x - eval y
  Mul x y -> eval x * eval y

