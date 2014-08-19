{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Proxy

class TypeCast (a :: k) (b :: k) | a -> b, b -> a where
  typeCast :: Proxy a -> Proxy b

class TypeCastWitness w (a :: k) (b :: k) | w a -> b, w b -> a where
  typeCastWitness :: w -> Proxy a -> Proxy b

class TypeCastContext w (a :: k) (b :: k) | w a -> b, w b -> a where
  typeCastWith :: w -> Proxy a -> Proxy b

instance TypeCastWitness () a b => TypeCast a b where
  typeCast = typeCastWitness ()

instance TypeCastContext t a b => TypeCastWitness t a b where
  typeCastWitness = typeCastWith

instance TypeCastContext () a a where
  typeCastWith _ x = x

class Case a (switch :: k) | a -> switch
instance TypeCast f True  => Case (x -> y) f
instance TypeCast f False => Case a f

class Foo a where
  foo :: a -> String

instance (Case a (f :: Bool), FooBranch f a) => Foo a where
  foo = foo' (Proxy :: Proxy f)

class FooBranch (f :: k) (a :: *) | f -> a where
  foo' :: Proxy f -> a -> String

instance FooBranch False a where
  foo' _ _ = "Not Function"

instance Case a (f :: Bool) => FooBranch True a where
  foo' _ _ = "Function"

demo1, demo2 :: String
demo1 = foo $ \x -> x
demo2 = foo ((\x -> x) :: a -> a)

main :: IO ()
main = do
  putStrLn demo1
  putStrLn demo2

