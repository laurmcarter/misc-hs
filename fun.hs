{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

data Z
data S a

class Peano n
instance Peano Z
instance (Peano n) => Peano (S n)

type family Add m n
type instance Add Z n = n
type instance Add (S m) n = Add m (S n)

type Zero = Z
type One = S Zero
type Two = S One
type Three = S Two
type Four = S Three
type Five = S Four

add :: (Peano m, Peano n) => m -> n -> Add m n
add = undefined

u = undefined

