import Data.Monoid

type HM m a b = ((m -> m) -> a) -> ((m -> m) -> b)

now :: Monoid m => m -> HM m a a 
now m k d = k (\m' -> d (m <> m'))

later :: Monoid m => (a -> m) -> HM m b (a -> b)
later am k d a = k (\m' -> d (am a <> m'))

run :: Monoid m => HM m m a -> a
run hm = hm ($ mempty) id

demo :: Int -> Int -> String
demo = run $ now "x = "
  . later show
  . now ", y = "
  . later show

demoUse = demo 10 20
