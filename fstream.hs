{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

newtype FStream a = SFK (forall ans . SK a ans -> FK ans -> ans)
type FK ans = () -> ans
type SK a ans = a -> FK ans -> ans
unSFK (SFK a) = a

newtype MStream m a = MSFK (forall ans . MSK m a ans -> MFK m ans -> m ans)
type MFK m ans = m ans
type MSK m a ans = a -> MFK m ans -> m ans

data Stream a = Nil | Cons a (() -> Stream a)

fstreamToStream :: FStream a -> Stream a
fstreamToStream fstream = unSFK fstream sk fk
  where
  fk () = Nil
  sk a fk' = Cons a fk'

streamToFStream :: Stream a -> FStream a
streamToFStream = \case
  Nil -> SFK $ \sk fk -> fk ()
  Cons a rest -> SFK $ \sk fk ->
    sk a $ \() -> unSFK (streamToFStream $ rest ()) sk fk

s1 = Cons 'a' $ \() -> Cons 'b' $ \() -> Cons 'c' $ \() -> Nil
f1 = streamToFStream s1
s1' = fstreamToStream f1

