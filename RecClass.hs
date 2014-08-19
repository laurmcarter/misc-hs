{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RecClass where

import GHC.Exts (Constraint)

class CShow (a :: *) where
  type ShowC a :: Constraint
  cShowsPrec :: ShowC a => Int -> a -> ShowS
  cShowsList :: ShowC a => Int -> [a] -> ShowS
  cShowsList _ as = showChar '[' . go as . showChar ']'
    where
    go l = case l of
      []   -> id
      a:l' -> cShowsPrec 0 a . showString ", " . go l'

instance CShow [a] where
  type ShowC [a] = (CShow a,ShowC a)
  cShowsPrec = cShowsList

instance CShow (a,b) where
  type ShowC (a,b) = (CShow a, CShow b, ShowC a, ShowC b)
  cShowsPrec _ (a,b) =
      showChar '('
    . cShowsPrec 0 a
    . showString ", "
    . cShowsPrec 0 b
    . showChar ')'

