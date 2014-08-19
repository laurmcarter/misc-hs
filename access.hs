
data X = X deriving (Eq,Show)
data Y = Y deriving (Eq,Show)
data Z = Z deriving (Eq,Show)


class GetX a where
  x :: a -> X
class GetY a where
  y :: a -> Y
class GetZ a where
  z :: a -> Z


data Foo = Foo
  { foo_x :: X
  , foo_y :: Y
  } deriving (Eq,Show)
data Bar = Bar
  { bar_x :: X
  , bar_z :: Z
  } deriving (Eq,Show)
data Baz = Baz
  { baz_y :: Y
  , baz_z :: Z
  } deriving (Eq,Show)


instance GetX Foo where
  x = foo_x
instance GetX Bar where
  x = bar_x

instance GetY Foo where
  y = foo_y
instance GetY Baz where
  y = baz_y

instance GetZ Bar where
  z = bar_z
instance GetZ Baz where
  z = baz_z


foo = Foo X Y
bar = Bar X Z
baz = Baz Y Z

