{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

data Nu f (as :: [*]) = In (f (Nu f as) as)

data (f :++: g) r (as :: [*])
  = Inl (f r as)
  | Inr (g r as)
  deriving Show

class (sub :: * -> [*] -> *) :<<: (sup :: * -> [*] -> *) where
  inj :: sub r as -> sup r as

instance f :<<: f where
  inj = id

instance f :<<: (f :++: g) where
  inj = Inl

instance (f :<<: g) => f :<<: (h :++: g) where
  inj = Inr . inj

inject :: (g :<<: f) => g (Nu f as) as -> Nu f as
inject = In . inj

type family Head (l :: [*])
type instance Head (a ': l) = a

type family Tail (l :: [*]) :: [*]
type instance Tail (a ': l) = l

data Cons r (as :: [*]) where
  Cons :: Head as -> r -> Cons r as

cons :: (Cons :<<: f) => Head as -> Nu f as -> Nu f as
cons a d = inject $ Cons a d

data Nil r (as :: [*]) where
  Nil :: Nil r as

nil :: (Nil :<<: f) => Nu f as
nil = inject Nil

data Foo r (as :: [*]) where
  Foo :: Head as -> Head (Tail as) -> Foo r as

foo :: (Foo :<<: f) => Head as -> Head (Tail as) -> Nu f as
foo x y = inject $ Foo x y

