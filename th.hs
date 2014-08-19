{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

data ABuilder a = AB { unAB :: StateT ATermTable IO a }

deriveShATermConvertible :: Name -> Q [Dec]
deriveShATermConvertible n = do
  inf <- reify n

