
import Control.Applicative

data FunList a b
  = Done b
  | More a (FunList a (a -> b))

getB :: FunList a b -> b
getB (Done b) = b
getB (More a z) = getB z a

getAs :: FunList a b -> [a]
getAs (Done b) = []
getAs (More a z) = a : getAs z

instance Functor (FunList a) where
  fmap f (Done b) = Done (f b)
  fmap f (More a z) = More a (fmap (f .) z)

instance Applicative (FunList a) where
  pure = Done
  (Done b) <*> c   = b <$> c
  (More a z) <*> c = More a (flip <$> z <*> c)

