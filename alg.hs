{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Control.Lens

class Group g where
  gempty    :: g
  ginverse  :: g -> g
  gmult     :: g -> g -> g

instance Group Integer where
  gempty = 0
  ginverse i = -i
  gmult = (+)

