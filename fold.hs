{-# LANGUAGE Arrows #-}

import Control.Arrow

--foldA :: Arrow arr => arr a (arr b a) -> arr a (arr [b] a)
foldA f = proc z -> do
  g <- f -< z
  f -< g

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z xs = case xs of
  []    -> z
  x:xs' -> foldl f (f z x) xs'

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = lgo z0 xs0
where
  lgo z []     =  z
  lgo z (x:xs) = lgo (f z x) xs
-}

