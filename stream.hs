{-# LANGUAGE Arrows #-}

import Prelude hiding (id,(.))
import Control.Category
import Control.Arrow.SP hiding (runSP)

foo :: SP IO Char Char
foo = id

