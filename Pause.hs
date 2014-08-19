
--module Pause where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

data PauseT m a
  = DoneT a
  | RunT (m (PauseT m a))

instance (Functor m) => Functor (PauseT m) where
  fmap f (DoneT a) = DoneT $ f a
  fmap f (RunT m) = RunT $ (fmap f) <$> m

instance (Monad m) => Monad (PauseT m) where
  return = DoneT
  DoneT a >>= f = f a
  RunT m  >>= f = RunT $ liftM (>>= f) m

instance MonadTrans PauseT where
  lift m = RunT $ liftM DoneT m

pause :: Monad m => PauseT m ()
pause = DoneT ()

joinP :: Monad m => PauseT m (PauseT m a) -> PauseT m a
joinP (DoneT m) = m
joinP (RunT mm) = RunT $ liftM (>>= id) mm

fullRunT :: PauseT IO r -> IO r
fullRunT (DoneT r) = return r
fullRunT (RunT m) = m >>= fullRunT

runNT :: Int -> PauseT IO r -> IO (PauseT IO r)
runNT 0 p = return p
runNT _ d@DoneT {} = return d
runNT n (RunT m) = m >>= runNT (n-1)

example2 :: PauseT IO ()
example2 = do
  lift $ putStrLn "Step 1"
  pause
  lift $ putStrLn "Step 2"
  pause
  lift $ putStrLn "Step 3"

newtype Coroutine s m r
  = Coroutine { resume :: m (CoroutineState s m r) }

data CoroutineState s m r
  = Run (s (Coroutine s m r))
  | Done r



