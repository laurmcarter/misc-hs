
import Prelude hiding ((.), id)
import Data.Function hiding ((.), id)

import Control.Category
import Control.Arrow
import Data.Monoid

newtype EndoCat cat a = EndoCat
  { appEndoCat :: cat a a
  }

instance (Category cat) => Monoid (EndoCat cat a) where
  mempty = EndoCat id
  mappend = EndoCat .: (.) `on` appEndoCat

type KEndo m = EndoCat (Kleisli m)

kEndo :: Monad m => (a -> m a) -> KEndo m a
kEndo = EndoCat . Kleisli

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

idM :: KEndo [] a
idM = mempty

dblM :: KEndo [] a
dblM = kEndo $ replicate 2

tplM :: KEndo [] a
tplM = kEndo $ replicate 3

