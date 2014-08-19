{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

foo = \case
  0 -> 0
  n -> n

bar x y = if
  | x < y     -> x
  | y < x     -> y
  | otherwise -> 0

