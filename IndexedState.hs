
module IndexedState where

import Prelude hiding (fmap, (>>=), (>>), return)

newtype IState i o a = IState
  { runIState :: i -> (a,o)
  }

evalIState :: IState i o a -> i -> a
evalIState m = fst . runIState m

execIState :: IState i o a -> i -> o
execIState m = snd . runIState m

return :: a -> IState s s a
return a = IState $ \s -> (a,s)

fmap :: (a -> b) -> IState i o a -> IState i o b
fmap f m = IState $ \i -> let (a,o) = runIState m i in (f a,o)

join :: IState i m (IState m o a) -> IState i o a
join m = IState $ \i -> uncurry runIState $ runIState m i

(>>=) :: IState i m a -> (a -> IState m o b) -> IState i o b
m >>= f = IState $ \i -> let (a,m') = runIState m i in runIState (f a) m'

(>>) :: IState i m a -> IState m o b -> IState i o b
m1 >> m2 = m1 >>= \_ -> m2

fail :: String -> IState i o a
fail = error

get :: IState s s s
get = IState $ \s -> (s,s)

put :: o -> IState i o ()
put o = IState $ const ((),o)

modify :: (i -> o) -> IState i o ()
modify f = IState $ \i -> ((), f i)

