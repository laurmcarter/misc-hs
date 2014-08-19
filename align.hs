
import Data.List

type Word = String
type Phrase = [Word]

data Context
  = Final Phrase
  | Hole Phrase Phrase Context

instance Show Context where
  show (Final p) = show p
  show (Hole p p1 c) = show (p1,p,c)

c1 = Hole ["test"] ["this","is","a"] $ Final []
c2 = Hole ["a"] ["this","is"] $ Hole ["test"] [] $ Final []

{-
fill1 :: Context -> Context
fill1 c = case c of
  Final p -> Final p
  Hole p f -> f p

fill :: Context -> Phrase
fill c = case c of
  Final p -> p
  Hole p f -> fill $ f p
-}

--align :: Phrase -> Phrase -> (Context,Context)
align p1 p2 = maximumPhrases $
  [ p
    | start <- [0..(l - 1)]
    , end   <- [0..(l - 1)]
    , (end - start) >= 0
    , let p = slice start end p1
    , p `isInfixOf` p2
  ]
  where
  l = length p1

{-
toContext :: Phrase -> Phrase -> [Phrase] -> (Context,Context)
toContext p1 p2 ps = case ps of
  []    -> (Final p1,Final p2)
  p:ps' -> 
-}

maximumPhrases :: [Phrase] -> [Phrase]
maximumPhrases = foldr maxPhrases []

maxPhrases :: Phrase -> [Phrase] -> [Phrase]
maxPhrases p ps = case ps of
  []    -> [p]
  p':ps' -> if p `isInfixOf` p'
    then ps
    else if p' `isInfixOf` p
      then p:ps'
      else p' : maxPhrases p ps'

slice :: Int -> Int -> Phrase -> Phrase
slice start end = drop start . take (end + 1)

x = words "this is a little test"
y = words "this is a payload"
z = words "this is the payload of an African Swallow"

