{-# LANGUAGE RankNTypes #-}

import Control.Applicative
import Control.Monad.Identity

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

-- Const a sewtype Const v a = Const { getConst :: v }
-- 
-- nstance Functor (Const v) where
--  fmap _ (Const v) = Const v

set :: forall s a. Lens s a -> a -> s -> s
set ln x = runIdentity . ln (Identity . const x)

over :: forall s a. Lens s a -> (a -> a) -> s -> s
over ln f = runIdentity . ln (Identity . f)

view :: forall s a. Lens s a -> s -> a
view ln = getConst . ln Const

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

infixr .:

