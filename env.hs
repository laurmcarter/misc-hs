
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader

import Data.Function (on)
import qualified Data.Map as M

type Env = M.Map String Int

lookup' :: Env -> String -> Maybe Int
lookup' e = flip M.lookup e

add :: String -> String -> Reader Env (Maybe Int)
add = addMaybes `onM` lookupInEnv
  where
  lookupInEnv :: String -> Reader Env (Maybe Int)
  lookupInEnv = (lookup' <$> ask <*^>)

  addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
  addMaybes = liftM2 (+)

(<*^>) :: (Applicative f) => f (a -> b) -> a -> f b
f <*^> a = f <*> pure a
infixl 4 <*^>

(<^*>) :: (Applicative f) => (a -> b) -> f a -> f b
f <^*> a = pure f <*> a
infixl 4 <^*>

onA :: (Applicative f) => (b -> b -> c) -> (a -> f b) -> a -> a -> f c
(op `onA` f) x y = op <$> f x <*> f y

onM :: (Monad m) => (b -> b -> c) -> (a -> m b) -> a -> a -> m c
(op `onM` f) x y = do
  x' <- f x
  y' <- f y
  return (x' `op` y')

