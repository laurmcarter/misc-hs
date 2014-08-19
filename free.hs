{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import System.Exit

-- Free {{{

data Free f a
  = Pure a
  | Free (f (Free f a))

instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
  Pure a == Pure b = a == b
  Free f == Free g = f == g
  _      == _      = False

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
  showsPrec d = \case
    Pure a -> showParen (d > 10) $ showString "Pure " . showsPrec 11 a
    Free f -> showParen (d > 10) $ showString "Free " . showsPrec 11 f

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free m) = Free (fmap f <$> m)

instance Functor f => Monad (Free f) where
  return  = Pure
  m >>= f = case m of
    Pure a  -> f a
    Free m' -> Free ((>>= f) <$> m')

instance Functor f => Applicative (Free f) where
  pure = return
  (<*>) = ap

cata :: Functor f => (f a -> a) -> Free f a -> a
cata phi = \case
  Pure a -> a
  Free m -> phi (cata phi <$> m)

liftF :: (Functor f, f :<: g) => f a -> Free g a
liftF = Free . fmap Pure . inj

-- }}}

data (f :+: g) a = L (f a) | R (g a) deriving (Eq,Show)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f = \case
    L e -> L (f <$> e)
    R e -> R (f <$> e)

infixr :+:

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance (Functor f) => f :<: f where
  inj = id
  prj = Just

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = L
  prj cp = case cp of
    L e -> Just e
    R _ -> Nothing

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = R . inj
  prj cp = case cp of
    L _ -> Nothing
    R e -> prj e

class (Functor f, Monad m) => Runnable f m where
  runF :: (Runnable g m) => f (Free g a) -> m a

instance (Runnable f m, Runnable g m) => Runnable (f :+: g) m where
  runF = \case
    L e -> runF e
    R e -> runF e

run :: (Runnable f m) => Free f a -> m a
run = \case
  Pure r -> return r
  Free f -> runF f

data Teletype a
  = Put FilePath a
  | Get (FilePath -> a)
  | Exit ExitCode
  deriving (Functor)

instance Runnable Teletype IO where
  runF = \case
    Put s t -> putStrLn s >> run t
    Get f   -> getLine >>= run . f
    Exit c  -> exitWith c

data FileSystem a
  = WriteFile FilePath String a
  | ReadFile FilePath (String -> a)
  deriving (Functor)

instance Runnable FileSystem IO where
  runF = \case
    WriteFile fp s t -> writeFile fp s >> run t
    ReadFile fp f    -> readFile fp >>= run . f

type TestIO = Free Teletype

put :: String -> TestIO ()
put s = liftF $ Put s ()

get :: TestIO String
get = liftF $ Get id

exit :: ExitCode -> TestIO ()
exit c = liftF $ Exit c

x :: TestIO ()
x = do
  put "foo"
  s <- get
  put s

