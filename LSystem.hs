{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}

-- LSys {{{

data LSys t n = LSys
  { lSys  :: [Either t n]
  , rules :: n -> [Either t n]
  }

instance (Show t, Show n) => Show (LSys t n) where
  show (LSys l _) = unwords $ map f l
    where
    f (Left t)  = show t
    f (Right t) = show t

step :: LSys t n -> LSys t n
step ls@(LSys l r) = ls { lSys = concatMap f l }
  where
  f (Left  t) = [Left t]
  f (Right n) = r n

stepN :: Int -> LSys t n -> [LSys t n]
stepN 0 ls  = [ls]
stepN n ls = ls : stepN (n-1) (step ls)

-- }}}

data AB
  = A
  | B
  deriving (Eq,Show)

a :: Either t AB
a = Right A
b :: Either t AB
b = Right B

-- Algae {{{

algae :: LSys () AB
algae = LSys
  { lSys  = [Right A]
  , rules = \case
               A -> [a,b]
               B -> [a]
  }

-- }}}

-- Ex 2 {{{

data PP
  = Push
  | Pop
  deriving (Eq,Show)

push :: Either PP n
push =  Left Push
pop  :: Either PP n
pop  =  Left Pop

ex2 :: LSys PP AB
ex2 = LSys
  { lSys  = [Right A]
  , rules = \case
               A -> [b,push,a,pop,a]
               B -> [b,b]
  }

-- }}}

-- Cantor Dust {{{

cantor :: LSys () AB
cantor = LSys
  { lSys  = [a]
  , rules = \case
               A -> [a,b,a]
               B -> [b,b,b]
  }

-- }}}



