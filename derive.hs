{-# LANGUAGE TemplateHaskell #-}

import Data.DeriveTH
import Data.Monoid

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq,Show)

$(derive makeFunctor ''List)

