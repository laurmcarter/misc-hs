
module IMonad
  ( IMonad (..)
  , foldlIM
  , IState (..)
  , evalIState
  , execIState
  , get
  , put
  , modify
  , ICont  (..)
  , callICC
  , IWriter (..)
  , tell
  , listen
  , execIWriter
  , FWriter
  , NonIndexed (..)
  , NonIxd
  , liftNI
  , ifThenElse
  , joinI
  ) where

import Prelude hiding ((>>=), (>>), fail, return, id, (.))
import qualified Prelude as Pr ((>>=), (>>), fail, return, id, (.))
import Control.Category

-- IMonad Class {{{

class IMonad m where
  return :: a -> m i i a
  (>>=) :: m i j a -> (a -> m j k b) -> m i k b
  (>>) :: m i j a -> m j k b -> m i k b
  m1 >> m2 = m1 >>= \_ -> m2
  fail :: String -> m i j a
  fail = error

foldlIM :: (Show a, Show b, IMonad m) => (a -> b -> m i i a) -> a -> [b] -> m i i a
foldlIM f a bs = case bs of
  []    -> return a
  b:bs' -> f a b >>= \a' -> foldlIM f a' bs'

joinI :: (IMonad m) => m i j (m j k a) -> m i k a
joinI m = m >>= id

-- }}}

-- IState {{{

newtype IState i j a  = IState { runIState :: i -> (j,a) }

instance Functor (IState i j) where
  fmap f m = IState $ fmap f . runIState m

instance IMonad IState where
  return a = IState $ \i -> (i,a)
  m >>= f  = IState $ \i -> let (j,a) = runIState m i in runIState (f a) j

get :: IState i i i
get = IState $ \i -> (i,i)

put :: j -> IState i j ()
put j = IState $ \_ -> (j,())

modify :: (i -> j) -> IState i j ()
modify f = IState $ \i -> (f i,())

evalIState :: IState i j a -> i -> a
evalIState m = snd . runIState m

execIState :: IState i j a -> i -> j
execIState m = fst . runIState m

-- }}}

-- ICont {{{

newtype ICont i j a = ICont { runICont :: (a -> j) -> i }

instance Functor (ICont i j) where
  fmap f m = ICont $ \k -> runICont m (k . f)

instance IMonad ICont where
  return a = ICont $ \k -> k a
  m >>= f  = ICont $ \k -> runICont m $ \a -> runICont (f a) k

callICC :: ((a -> ICont j k b) -> ICont i j a) -> ICont i j a
callICC f = ICont $ \k -> runICont (f $ \a -> ICont $ \_ -> k a) k

-- }}}

-- IWriter {{{

newtype IWriter cat s1 s2 a = IWriter { runIWriter :: (cat s1 s2, a) }

instance (Category cat) => Functor (IWriter cat s1 s2) where
  fmap f = IWriter . fmap f . runIWriter

instance (Category cat) => IMonad (IWriter cat) where
  return a = IWriter (id,a)
  m >>= f  = let (w,a)  = runIWriter m
                 (w',b) = runIWriter (f a)
               in IWriter (w' . w, b)

tell :: Category cat => cat s1 s2 -> IWriter cat s1 s2 ()
tell w = IWriter (w,())

listen :: Category cat => IWriter cat s1 s2 a -> IWriter cat s1 s2 (cat s1 s2)
listen w = let f = execIWriter w in IWriter (f,f)

execIWriter :: Category cat => IWriter cat s1 s2 a -> cat s1 s2
execIWriter = fst . runIWriter

type FWriter = IWriter (->)

-- }}}

-- NonIndexed {{{

newtype NonIndexed m i j a = NonIxd { runNonIxd :: m a }
type NonIxd m a = NonIndexed m () () a

instance (Monad m) => IMonad (NonIndexed m) where
  return = NonIxd . Pr.return
  (NonIxd m) >>= f = NonIxd $ (Pr.>>=) m $ \a -> runNonIxd $ f a
  fail = NonIxd . Pr.fail

liftNI :: (Monad m) => m a -> NonIndexed m i j a
liftNI = NonIxd

-- }}}

ifThenElse b x y = if b then x else y

