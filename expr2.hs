{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

newtype Expr f = In (f (Expr f))

foldExpr :: (Functor f) => (f a -> a) -> Expr f -> a
foldExpr f (In e) = f (fmap (foldExpr f) e)

data (f :+: g) e = Inl (f e) | Inr (g e) deriving Show

newtype HJust a = HJust a
data HNothing a = HNothing

class IsSub f g result | f g -> result where
  ginj :: result (f a -> g a)

instance IsSub f f HJust where
  ginj = HJust id

instance (IsSub f g b, IsSubL b f g h result) => IsSub f (g :+: h) result where
  ginj = ginjL (ginj :: b (f a -> g a))

class IsSubL b f g h result | b f g h -> result where
  ginjL :: b (f a -> g a) -> result (f a -> (g :+: h) a)

instance IsSubL HJust f g h HJust where
  ginjL (HJust tr) = HJust (Inl . tr)

instance (IsSub f h b, IsSubR b f g h result) => IsSubL HNothing f g h result where
  ginjL _ = ginjR (ginj :: b (f a -> h a))

class IsSubR b f g h result | b f g h -> result where
  ginjR :: b (f a -> h a) -> result (f a -> (g :+: h) a)

instance IsSubR HJust f g h HJust where
  ginjR (HJust tr) = HJust (Inr . tr)

instance IsSubR HNothing f g h HNothing where
  ginjR _ = HNothing

data Val a = Val Int deriving Show
data Add a = Add a a deriving Show
data Mul a = Mul a a deriving Show

instance Functor Val where
  fmap f (Val x) = Val x
instance Functor Add where
  fmap f (Add x y) = Add (f x) (f y)
instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)

instance IsSub Val Add HNothing where ginj = HNothing
instance IsSub Val Mul HNothing where ginj = HNothing
instance IsSub Add Val HNothing where ginj = HNothing
instance IsSub Add Mul HNothing where ginj = HNothing
instance IsSub Mul Val HNothing where ginj = HNothing
instance IsSub Mul Add HNothing where ginj = HNothing

--x :: Expr Val
--x = val 1
--
--y :: Expr (Val :+: Add)
--y = val 1

