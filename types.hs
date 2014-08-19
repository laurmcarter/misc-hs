{-# LANGUAGE StandaloneDeriving, DeriveFunctor, FlexibleInstances, UndecidableInstances, ImplicitParams #-}

import Data.Char
import Control.Monad.Identity

data Mu f = Mu (f (Mu f))

deriving instance Show (f (Mu f)) => Show (Mu f)

data Located f a = f a :+ (Int, Int)

deriving instance Show (f a) => Show (Located f a)

data AST ast = Val String | Apply ast ast deriving (Eq, Show, Functor)

type AST' = Mu (Located AST)

deriving instance Show a => Show (Identity a)

foo = Mu ((Apply (Mu ((Val "foo") :+ (0,1))) (Mu ((Val "bar") :+ (1,0)))) :+ (0,0))

