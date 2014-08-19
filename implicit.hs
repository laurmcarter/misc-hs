{-# LANGUAGE ImplicitParams #-}

foo :: (?hole :: Int) => Int
foo = ?hole

bar :: Int
bar = let ?hole = 1 in foo

baz :: Int
baz = let ?hole = 2 in foo

