{-# LANGUAGE ExistentialQuantification #-}

data Trans a b
  = Final (a -> b)
  | forall c. Step (a -> c) (Trans c b)


