
import Control.Arrow

-- Wr {{{1

data Wr a = Wr
  { msg :: String
  , val :: a
  }

instance Show a => Show (Wr a) where
  show w = "Message:\n" ++ msg w ++ "\n" ++ (show $ val w)

instance Functor Wr where
  fmap f w = w { val = f $ val w }

instance Monad Wr where
  return a = Wr { msg = "", val = a }
  w >>= f  = let w' = f $ val w in w' { msg = msg w ++ msg w' }

tell :: String -> Wr ()
tell s = Wr { msg = s, val = () }

-- List {{{1

cataListM :: (Show a, Show b) => (a -> b -> Wr b) -> b -> [a] -> Wr b
cataListM f b l = case l of
  []     -> return b
  (a:l') -> cataListM f b l' >>= f a

cataListM' :: (Show a, Show b) => (b -> a -> Wr b) -> b -> [a] -> Wr b
cataListM' f b l = case l of
  []     -> return b
  (a:l') -> do b' <- f b a
               cataListM' f b' l'

cataList :: (a -> b -> b) -> b -> [a] -> b
cataList f b l = case l of
  []     -> b
  (a:l') -> f a $ cataList f b l'

cataList' :: (b -> a -> b) -> b -> [a] -> b
cataList' f b l = case l of
  []     -> b
  (a:l') -> cataList' f (f b a) l'

-- Tree {{{1

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

cataTreeM brF lF b t = case t of
  Leaf a       -> lF a b
  Branch l r -> do tell "entering left\n"
                   l' <- cataTreeM brF lF b l
                   tell "entering right\n"
                   r' <- cataTreeM brF lF b r
                   tell "resurfacing\n"
                   brF l' r'

cataTreeM' brF lF b t = case t of
  Leaf a       -> lF a b
  Branch l r -> do l' <- cataTreeM' brF lF b l
                   r' <- cataTreeM' brF lF b r
                   brF l' r'

-- }}}

hylo :: (a -> Bool) -> (b -> c -> c) -> (a -> (b,a)) -> c -> a -> c
hylo stop join split end a = if stop a
  then end
  else join b $ hylo stop join split end a'
    where (b,a') = split a

fac = hylo (== 0) (*) (\a->(a,a-1)) 1

plusM :: (Show a, Num a) => a -> a -> Wr a
plusM x y = let z = x + y in do
  tell (show x ++ " + " ++ show y ++ " = " ++ show z ++ "\n")
  return z

