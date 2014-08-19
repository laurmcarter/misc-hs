{-# LANGUAGE MagicHash #-}

import GHC.Num
import GHC.Exts

hash :: Integer -> Int
hash i = I# (hashInteger i)

