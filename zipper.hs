{-# LANGUAGE LambdaCase #-}

import Control.Monad

-- {{{

{-
data Tree a
  = Leaf a
  | Bin (Tree a) (Tree a)
  deriving (Eq,Show)

data Back a
  = DownL (Tree a)
  | DownR (Tree a)
  deriving (Eq,Show)

type Thread a = [Back a]
type Zipper a = (Thread a,Tree a)

toZipper :: Tree a -> Zipper a
toZipper t = ([],t)

downLeft :: Zipper a -> Maybe (Zipper a)
downLeft (th,tr) = case tr of
  Bin l r -> Just (DownL r : th,l)
  _       -> Nothing

downRight :: Zipper a -> Maybe (Zipper a)
downRight (th,tr) = case tr of
  Bin l r -> Just (DownR l : th,r)
  _       -> Nothing

back :: Zipper a -> Maybe (Zipper a)
back (th,tr) = case th of
  DownL r : th' -> Just $ (th',Bin tr r)
  DownR l : th' -> Just $ (th',Bin l tr)
  _             -> Nothing

allTheWayUp :: Zipper a -> Tree a
allTheWayUp z@(th,tr) = maybe tr allTheWayUp $ back z

change :: a -> Zipper a -> Maybe (Zipper a)
change a (th,tr) = case tr of
  Leaf _ -> Just (th,Leaf a)
  _      -> Nothing

tree1 :: Tree Int
tree1 = Bin
  (Bin
    (Leaf 1)
    (Leaf 2))
  (Bin
    (Bin
      (Leaf 3)
      (Leaf 4))
    (Bin
      (Leaf 5)
      (Bin
        (Leaf 6)
        (Leaf 7))))

zip1 :: Zipper Int
zip1 = toZipper tree1

($$) :: a -> (a -> b) -> b
x $$ f = f x

infixr 0 $$
-}

-- }}}



