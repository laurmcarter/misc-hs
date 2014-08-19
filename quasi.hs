{-# LANGUAGE TemplateHaskell #-}

import Projections

import Control.Lens
import Data.Map

data Foo
  = Foo (Maybe Foo)
  deriving (Eq,Show)

data Bar
  = Bar Int Bool
  | Bar'
  deriving (Eq,Show)

data JSON
  = JMap (Map String JSON)
  | JArray [JSON]
  | JString String
  | JInt Int
  | JBool Bool
  | JNull
  deriving (Eq,Show)

makeProjections ''Foo
makeProjections ''Bar
makeProjections ''JSON

makePrisms ''Foo
makePrisms ''Bar
makePrisms ''JSON

