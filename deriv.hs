{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-
type family Derivative (f :: * -> *) :: * -> *

data Void
data Unit = Unit                           deriving (Eq,Show)
data Prod f g a = Prod (f a) (g a)         deriving (Eq,Show)
data Sum f g a = InL (f a) | InR (g a)     deriving (Eq,Show)
data Comp f g a = Comp (f (g a))           deriving (Eq,Show)

type instance Derivative (Sum f g)  = Sum (Derivative f) (Derivative g)
type instance Derivative (Prod f g) = Sum (Prod (Derivative f) g) (Prod f (Derivative g))
type instance Derivative (Comp f g) = Prod (Derivative g) (Comp (Derivative f) g)
-}

data Id a = Id a deriving (Eq,Show)
data Add f g a = L (f a) | R (g a) deriving (Eq,Show)
data Mul f g a = M (f a) (g a) deriving (Eq,Show)

type family Prod (fs :: [* -> *]) a
type instance Prod '[] a = a
type instance Prod (f ': fs) a = Mul f (Prod fs) a

newtype Fix f = Fix (f (Fix f))


