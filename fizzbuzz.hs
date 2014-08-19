
import Control.Monad (forM_)

type Env a = [(a,String)]

env :: (Show a, Integral a) => Env a
env = [ (2,"Fooz")
      , (3,"Fizz")
      , (5,"Buzz")
      , (7,"Bazz")
      , (11,"Quux")
      , (13,"Boogaloo")
      , (17,"Bangarang")
      , (23,"Smorgasbord")
      ]

fizzbuzz :: (Show a, Integral a) => Env a -> a -> String
fizzbuzz e i = case concatMap snd [ x | x <- e , (i `mod` fst x) == 0 ] of
  "" -> show i
  s  -> s

main = forM_ [0..100] (putStrLn . fizzbuzz env)
