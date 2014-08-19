
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graphviz

import System.Process
import System.Exit
import qualified Data.Map as M

-- Reading List -----------------------------------------------------

readingList =
  [ ("<Linear>"    , [ "Herstein" ])
  , ("Herstein"    , [ "Ash" ])
  , ("Ash"         , [])
  , ("LambdaPi"    , [ "Cryptol" ])
  , ("Cryptol"     , [ "Keccak" ])
  , ("Keccak"     , [])
  ]

---------------------------------------------------------------------

type GList = [(String,[String])]
readingList :: GList

buildGraph :: [(String,[String])] -> Gr String ()
buildGraph gs = mkGraph ns es
  where
    nodeMap = buildMap ns
    es      = lookupNodes nodeMap $ elab gs
    ns      = indexNodes s
    s       = map fst gs

lookupNodes :: M.Map String Int -> [(String,String)] -> [LEdge ()]
lookupNodes m es = case es of
  []        -> []
  (f,t):es' -> (m M.! f, m M.! t,()) : lookupNodes m es'

elab :: [(a,[b])] -> [(a,b)]
elab ls = concatMap (\(a,bs)->(map (\b->(a,b)) bs)) ls

buildMap :: [LNode String] -> M.Map String Int
buildMap ns = case ns of
  []        -> M.empty
  (i,s):ns' -> M.insert s i $ buildMap ns'

indexNodes :: [a] -> [LNode a]
indexNodes = indexNodes' 0

indexNodes' :: Int -> [a] -> [LNode a]
indexNodes' i ns = case ns of
  []      -> []
  (n:ns') -> (i,n) : indexNodes' (i+1) ns'

main :: IO ()
main = do
  readProcess "dot" ["-T","gif","-o","test.gif"] $ graphviz (buildGraph readingList) "Test" (10,10) (1,1) Portrait
  return ()
