{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative hiding (empty)
import Data.Set as S
import qualified Data.Map as M
import Data.Maybe

subsequences :: Ord a => Set a -> Set (Set a)
subsequences xs = empty `insert` nonEmptySubsequences xs

nonEmptySubsequences :: Ord a => Set a -> Set (Set a)
nonEmptySubsequences xs = if S.null xs
  then empty
  else let (x,xs') = deleteFindMin xs
           f ys r  = insert ys $ insert (insert x ys) r
    in singleton x `insert` S.foldr f empty (nonEmptySubsequences xs')

type Prob = Double
type Mass a = Set a -> MassMap a
type MassMap a = M.Map (Set a) Prob

equiv :: Double -> Double -> Bool
x `equiv` y = abs (x - y) < 1e-7

verifyMap :: Ord a => MassMap a -> Bool
verifyMap m = (Just True == ((== 0) <$> M.lookup empty m)) &&
  (1 `equiv` sum (M.elems m))



