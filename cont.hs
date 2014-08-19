
import Control.Applicative
import Control.Monad

newtype E r a = E { runE :: (String -> r) -> (a -> r) -> r }

instance Functor (E r) where
  fmap f m = E $ \ fl sc -> runE m fl (sc . f)

instance Monad (E r) where
  return a = E $ \ _  sc -> sc a
  m >>= f  = E $ \ fl sc ->
    runE m fl (\a -> runE (f a) fl sc)
  fail str = E $ \ fl _  -> fl str

instance MonadPlus (E r) where
  mzero = fail "mzero"
  m1 `mplus` m2 = E $ \ fl sc ->
    runE m1 (\str -> runE m2 fl sc) sc

instance Applicative (E r) where
  pure = return
  (<*>) = ap

instance Alternative (E r) where
  empty = mzero
  (<|>) = mplus

x :: E (IO ()) Int
x = fail "failed"

y :: E (IO ()) String
y = do
  b <- x <|> return 2
  return $ show b

