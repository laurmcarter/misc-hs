
import System.IO
import Control.Monad
import Control.Applicative
import Data.Maybe

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (not . null) . lines

parseLine :: String -> Maybe (Double,Bool)
parseLine s = if length ws == 2
  then let hours = read $ head ws :: Double
           billable = read $ ws !! 1 :: Bool in
    Just (hours,billable)
  else Nothing
  where
  ws = words s

sortHours :: (Double,Double) -> (Double,Bool) -> (Double,Double)
sortHours (b,n) (hs,isBillable) = if isBillable
  then (b+hs,n)
  else (b,n+hs)

accumHours :: [(Double,Bool)] -> (Double,Double)
accumHours = foldl sortHours (0,0)

main :: IO ()
main = do
  ss <- nonEmptyLines <$> readFile "time.txt"
  let (b,n) = accumHours $ mapMaybe parseLine ss
  putStrLn ("Billable: " ++ show b)
  putStrLn ("Nonbillable: " ++ show n)
  putStrLn "======================="
  putStrLn ("Ratio: " ++ show (b / (b + n)))

