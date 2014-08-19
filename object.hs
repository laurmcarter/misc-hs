{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

import Language.Haskell.Codo
import Control.Arrow (first,(***))
import Control.Comonad
import Text.Show.Pretty

import Data.Monoid

-- Comonad {{{

replicateW :: Comonad w => Int -> (w a -> a) -> w a -> a
replicateW n f
  | n == 0    = extract
  | otherwise = f =>= replicateW (n - 1) f

-- }}}

-- Builder {{{

type Option = String

type ConfigBuilder = Builder [Option]

data Config = MkConfig [Option]
  deriving (Show)

data Builder b a = Builder
  { runBuilder :: b -> a
  }

instance Functor (Builder b) where
  fmap f bldr = Builder (f . runBuilder bldr)

instance Monoid b => Comonad (Builder b) where
  extract b = runBuilder b mempty
  f `extend` b = Builder $ \m' ->
    f $ Builder $ \m -> runBuilder b (m <> m')

configBuilder :: ConfigBuilder Config
configBuilder = Builder MkConfig

defaultConfig :: ConfigBuilder Config
defaultConfig = configBuilder =>> buildDefault

buildDefault :: ConfigBuilder Config -> Config
buildDefault bldr = runBuilder bldr ["-Wall"]

profile :: ConfigBuilder Config -> Config
profile bldr = runBuilder bldr ["-prof","-auto-all"]

goFaster :: ConfigBuilder Config -> Config
goFaster bldr = runBuilder bldr ["-O2"]

profile' :: ConfigBuilder Config -> ConfigBuilder Config
profile' = extend profile

goFaster' :: ConfigBuilder Config -> ConfigBuilder Config
goFaster' = extend goFaster

-- }}}

-- Iterator {{{

data Iterator a = a :< (Iterator a)

infixr 0 :<

instance Functor Iterator where
  fmap f (a :< as) = f a :< fmap f as

instance Comonad Iterator where
  extract (a :< _) = a
  f `extend` i@(_ :< as) = f i :< extend f as

next :: Iterator a -> a
next (_ :< as) = extract as

next' :: Iterator a -> Iterator a
next' = extend next

initialHistory :: Iterator String
initialHistory = "" :< initialHistory

exampleHistory :: Iterator String
exampleHistory
  =  "^D"
  :< "^C"
  :< "eat flaming death"
  :< "hello?"
  :< "bye"
  :< "exit"
  :< "quit"
  :< "?"
  :< "help"
  :< "ed"
  :< initialHistory

-- }}}

-- Context {{{

newtype Kelvin = Kelvin
  { kelvin :: Double
  } deriving (Eq,Show)

newtype Celsius = Celsius
  { celsius :: Double
  } deriving (Eq,Show)

data Context c a = Context c (c -> a)

instance Functor (Context c) where
  fmap g (Context c f) = Context c (g . f)

instance Comonad (Context c) where
  extract (Context c f) = f c
  g `extend` (Context c f) = Context c $ \c' -> g (Context c' f)

initialThermostat :: Context Kelvin Celsius
initialThermostat = Context (Kelvin 298.15) kelvinToCelsius

kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius k = Celsius $ kelvin k - 273.15

up :: Context Kelvin a -> a
up (Context k f) = f $ Kelvin $ kelvin k + 1

down :: Context Kelvin a -> a
down (Context k f) = f $ Kelvin $ kelvin k - 1

-- }}}

data LGraph a = LG [(a,[Int])] Int deriving (Eq,Show)

instance Functor LGraph where
  fmap f (LG xs c) = LG (map (first f) xs) c

instance Comonad LGraph where
  extract (LG xs c) = fst (xs !! c)
  extend f (LG xs c) = LG (map (\c' -> (f (LG xs c'),snd (xs !! c))) [0..length xs]) c

data BTree a
  = Leaf a
  | Node a (BTree a) (BTree a)
  deriving (Eq,Show)

instance Functor BTree where
  fmap f t = case t of
    Leaf a     -> Leaf $ f a
    Node a l r -> Node (f a) (fmap f l) (fmap f r)

instance Comonad BTree where
  extract t = case t of
    Leaf a     -> a
    Node a _ _ -> a
  extend f t = case t of
    Leaf a     -> Leaf $ f $ Leaf a
    Node a l r -> Node (f t) (extend f l) (extend f r)

testTree :: BTree Int
testTree = Node 1
  (Node 2
    (Leaf 3)
    (Leaf 4))
  (Node 5
    (Leaf 6)
    (Node 7
      (Leaf 8)
      (Leaf 9)))

