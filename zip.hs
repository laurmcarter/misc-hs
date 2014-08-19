{-# LANGUAGE PatternGuards #-}

import Data.Sequence (ViewL(..),(<|),Seq)
import Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Traversable as T

class Functor f => Zip f where
  fzipWith :: (a -> b -> c) -> f a -> f b -> f c
  
  fzip :: f a -> f b -> f (a,b)
  fzip = fzipWith (,)

  funzip :: f (a, b) -> (f a, f b)
  funzip fab = (fmap fst fab, fmap snd fab)

instance Zip [] where
  fzipWith f xs ys = case (xs,ys) of
    ([],[]) -> []
    (x:xs',y:ys') -> f x y : fzipWith f xs' ys'
    _ -> []

instance Zip Seq where
  fzipWith f xs ys = case (S.viewl xs, S.viewl ys) of
    (EmptyL,EmptyL) -> S.empty
    (x :< xs', y :< ys') -> f x y <| fzipWith f xs' ys'
    _ -> S.empty

instance Zip Maybe where
  fzipWith f mx my = case (mx,my) of
    (Nothing,Nothing) -> Nothing
    (Just x,Just y) -> Just $ f x y
    _ -> Nothing

{-
instance (Monoid a) => Zip (Either a) where
  fzipWith f ex ey = case (ex,ey) of
    (Left a, Left b) -> Left (a <> b)
    (Right a, Left b) -> Left a
    (Left a, Right b) -> Left b
    (Right a, Right b) -> Right $ f a b
-}

{-
instance (Monoid a) => Zip ((,) a) where
  fzipWith f (a,c) (b,d) = (a <> b, f c d)
-}

