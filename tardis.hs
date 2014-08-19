
data Tardis bw fw a = Tardis { runTardis :: (bw,fw) -> (a,bw,fw) }

instance Functor (Tardis b f) where
  fmap f m = Tardis $ \(bw,fw) -> let (a,bw',fw') = runTardis m (bw,fw) in (f a,bw',fw')

instance Monad (Tardis b f) where
  return a = Tardis $ \(bw,fw) -> (a,bw,fw)
  m >>= f  = Tardis $ \(bw,fw) ->
    let (a1,bw1,fw1) = runTardis m (bw2,fw)
        (a2,bw2,fw2) = runTardis (f a1) (bw,fw1) in
      (a2,bw1,fw2)

evalTardis :: Tardis bw fw a -> (bw,fw) -> a
evalTardis m is = let (a,_,_) = runTardis m is in a

getBW :: Tardis bw fw bw
getBW = Tardis $ \(bw,fw) -> (bw,bw,fw)

getFW :: Tardis fw fw bw
getFW = Tardis $ \(bw,fw) -> (fw,bw,fw)

