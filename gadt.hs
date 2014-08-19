{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

data UpStar f d c where
  UpStar :: Functor f => d -> f c 

