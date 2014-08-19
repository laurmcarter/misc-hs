

-- UUAGC 0.9.42.1 (tree.ag)

{-# LINE 14 "./tree.ag" #-}

main :: IO ()
main = print $ show test

testTree :: Tree
testTree = Node (Tip 1) (Node (Tip 2) (Tip 3))

test :: Int
test = sem_Tree testTree
{-# LINE 16 "tree.hs" #-}
-- Tree --------------------------------------------------------
data Tree = Node (Tree) (Tree)
          | Tip (Int)
-- cata
sem_Tree (Node _left _right) =
    (sem_Tree_Node (sem_Tree _left) (sem_Tree _right))
sem_Tree (Tip _value) =
    (sem_Tree_Tip _value)
sem_Tree_Node left_ right_ =
    (let _lhsOsum =
             ({-# LINE 11 "./tree.ag" #-}
              _leftIsum + _rightIsum
              {-# LINE 29 "tree.hs" #-}
              )
         ( _leftIsum) =
             left_
         ( _rightIsum) =
             right_
     in  ( _lhsOsum))
sem_Tree_Tip value_ =
    (let _lhsOsum =
             ({-# LINE 12 "./tree.ag" #-}
              value_
              {-# LINE 40 "tree.hs" #-}
              )
     in  ( _lhsOsum))