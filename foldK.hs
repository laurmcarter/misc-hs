
import Control.Monad.Trans.Cont
import Debug.Trace

import ContAbort

foldlK' :: (a -> b -> ContAbort a a a) -> a -> [b] -> a
foldlK' f a bs = runCont k id
  where
  k = cont $ \out -> runContAbort (go a bs) out out
  go a bs = case bs of
    []    -> return a
    b:bs' -> do
      a' <- f a b
      go a' bs'

foldrK' :: (a -> b -> ContAbort b b b) -> b -> [a] -> b
foldrK' f b as = runCont k id
  where
  k = cont $ \out -> runContAbort (go b as) out out
  go b as = case as of
    []    -> return b
    a:as' -> do
      b' <- go b as'
      f a b'

prod' = foldlK' $ \a b -> if b == 0
  then abort 0
  else return $ times a b

keepUntil :: (a -> Bool) -> [a] -> [a]
keepUntil pr = flip foldlK' [] $ \as a ->
  if pr a
  then abort as
  else return (as++[a])

times :: Int -> Int -> Int
times x y = trace (show x ++ " * " ++ show y) (x * y)

