{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

data Nat
  = Z
  | S Z

data Vec (n :: Nat) a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

data TCInt (n :: Nat) = TCInt
  { bits :: Vec n Bool
  }
  deriving (Eq,Show)

safeHead :: [a] -> Maybe a
safeHead l = case l of
  a:_ -> Just a
  _   -> Nothing

safeTail :: [a] -> Maybe [a]
safeTail l = case l of
  _:l' -> Just l'
  _    -> Nothing

instance Num TCInt where
  ...

