
data Workring
  = Ring Workring
  | Window Int

data Ring a
  = R (Ring a) a (Ring a)

fromList :: [a] -> Ring a
fromList [] = error "must have at least one element"
fromList xs = let (first,last) = go last xs first
              in  first

go :: Ring a -> [a] -> Ring a -> (Ring a, Ring a)
go prev []     next = (next,prev)
go prev (x:xs) next = let this        = R prev x rest
                          (rest,last) = go this xs next
                      in  (this,last)

takeF :: Integer -> Ring a -> [a]
takeF 0     _                 = []
takeF n (R _ x next) = x : (takeF (n-1) next)
 
takeR :: Show a => Integer -> Ring a -> [a]
takeR 0     _                 = []
takeR n (R prev x _) = x : (takeR (n-1) prev)
