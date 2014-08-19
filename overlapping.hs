{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import TTypeable

class ShowList a where
  showl :: [a] -> String

type Special = TYPEOF Bool :/ TYPEOF Char :/ NIL

instance (flag ~ (Member AC_TREPEQ (TYPEOF a) Special), ShowList' flag a)
  => ShowList a where
  showl = showl' (undefined :: flag)

class ShowList' flag a where
  showl' :: flag -> [a] -> String

instance Show a => ShowList' HFalse a where
  showl' _ [] = "[]"
  showl' _ xs = "[" ++ concat (intersperse "," (map show xs)) ++ "]"

instance ShowList' HTrue Bool where
  showl' _ xs = concatMap toBit xs
    where
    toBit False = "0"
    toBit True  = "1"

instance ShowList' HTrue Char where
  showl' _ = id

