{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Monad

foo :: Arrow a => a Integer Integer
foo = proc x -> do
  y <- f -< x
  let z = x+y
  t <- h -< x*z
  returnA -< t+z

f,h :: Arrow a => a Integer Integer
f = arr (+1)
h = arr (+3)

g :: Kleisli [] Integer Integer
g = Kleisli $ \a -> replicate (fromInteger a) a

bar :: Kleisli [] Integer Integer
bar = foo

