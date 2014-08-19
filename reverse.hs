
newtype Reverse s a = Reverse { runReverse :: s -> (a,s) }

instance Functor (Reverse s) where
  fmap f m = Reverse $ \s -> let (a,s') = runReverse m s in (f a,s')

instance Monad (Reverse s) where
  return a = Reverse $ \s -> (a,s)
  m >>= f  = Reverse $ \s -> let (a,s') = runReverse m s in runReverse (f a) s'

gets :: (s -> b) -> Reverse s b
gets f = Reverse $ \s -> (f s,s)

get = gets id

put :: s -> Reverse s ()
put s = Reverse $ \_ -> ((),s)

modify :: (s -> s) -> Reverse s ()
modify f = do s <- get
              put $ f s

xor :: Bool -> Bool -> Bool
x `xor` y = case (x,y) of
  (True,True)   -> False
  (True,False)  -> True
  (False,True)  -> True
  (False,False) -> False

