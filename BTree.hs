
data BTree a
  = Leaf
  | Node a (BTree a) (BTree a)
  deriving (Eq,Show)

foldBTree :: r -> (a -> r -> r -> r) -> BTree a -> r
foldBTree leaf node bt = case bt of
  Leaf -> leaf
  Node a l r -> node a (rec l) (rec r)
  where
  rec = foldBTree leaf node

