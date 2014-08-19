import System.IO
import Data.List
import Data.Function
import Data.Maybe
import Control.Arrow
import Control.Monad

type FreqData = (Char,Int)
type CodeData = (Char,[Int])

countChar :: String -> [FreqData]
countChar = map (head &&& length) . group . sort

sortData :: [FreqData] -> [FreqData]
sortData = sortBy (compare `on` snd)

data Tree a = Leaf Int a | Node Int (Tree a) (Tree a) deriving (Show)

freq :: Tree a -> Int
freq (Leaf p _)   = p
freq (Node p _ _) = p

buildTree :: [Tree a] -> (Tree a, [Tree a])
buildTree [] = error "Cannot buildTree from empty list"
buildTree (t:ts) = tree t [] ts
  where tree x xs [] = (x,xs)
        tree x xs (y:ys)
          | freq y < freq x = tree y (x:xs) ys
          | otherwise       = tree x (y:xs) ys

getTree :: [FreqData] -> Tree Char
getTree = build . map (\(t,p) -> Leaf p t)
  where build [t] = t
        build ts = 
          let (t0,ts0) = buildTree ts
              (t1,ts1) = buildTree ts0
          in  build $ Node (freq t0 + freq t1) t0 t1 : ts1

encode :: Tree Char -> [CodeData]
encode = prefx []
  where prefx bs (Leaf _ x) = [(x,bs)]
        prefx bs (Node _ t0 t1) = prefx (bs ++ [0]) t0 ++ prefx (bs ++ [1]) t1

compress :: [CodeData] -> String -> [Int]
compress cs = concatMap (\x -> fromJust $ lookup x cs)

decompress :: [CodeData] -> Int -> [Int] -> String
decompress _ _ [] = []
decompress dict ix bs
  | c == take n bs = ch : decompress dict 0 (drop n bs)
  | otherwise      = decompress dict (ix+1) bs
  where (ch,c) = dict!!ix
        n = length c

putListLn :: (Show a) => [a] -> IO ()
putListLn xs = mapM_ putStr $ map show xs ++ ["\n"]

putMapLn :: (Show a) => (Char, [a]) -> IO ()
putMapLn (ch,c) = do
  putStr $ ch : " : "
  putListLn c

prompt :: String -> IO ()
prompt msg = do
  putStr msg
  hFlush stdout

main :: IO ()
main = do
  void $ forever $ do
    prompt "Enter string to be compressed: "
    str <- getLine
    let charFreq = sortData $ countChar str
        codeDict = encode $ getTree charFreq
        bitStr = compress codeDict str
        reStr = decompress codeDict 0 bitStr
    putStrLn "\nCharacter Key:"
    mapM_ putMapLn codeDict
    putStrLn "\nCompressed:"
    putListLn bitStr
    putStrLn $ "\nDecompressed:\n" ++ reStr ++ "\nPress [ENTER] to repeat"
    getLine
  return ()
