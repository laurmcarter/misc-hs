
import Control.Monad.Writer
import Control.Monad.State
import Graphics.Gloss.Interface.IO.Game

import Turtle

type World = IO (Picture,[TurtleRef])

world :: Picture -> [TurtleRef] -> World
world p ts = return (p,ts)

stepWorld :: TurtleM a -> World -> IO World
stepWorld m w = do
  (p,ts)     <- w
  (_,p',ts') <- runTurtleM ts m
  return $ world (p <> p') ts'

renderWorld :: World -> IO Picture
renderWorld = fmap fst

emptyWorld = return (blank,[])

initWorld :: World
initWorld = join $ stepWorld setup emptyWorld
  where
  setup = do
    newTurtleWithColor red
    newTurtleWithColor yellow >>= left 90
    newTurtleWithColor green  >>= left 180
    newTurtleWithColor blue   >>= left 270
    allTurtles
      [ drawArc ToLeft  180 80
      , drawArc ToRight 180 40
      , drawArc ToLeft  180 20
      , drawArc ToRight 180 10
      , drawArc ToLeft  180  5
      , drawArc ToRight 180  2.5
      , drawArc ToLeft  180  1.25
      , drawArc ToRight 180  0.625
      ]

oneStep :: World -> IO World
oneStep = stepWorld $ do
  allTurtles
    [ left 100
    , forward 200
    ]

waitWorld :: World -> IO World
waitWorld = return

handleEvents :: Event -> World -> IO World
handleEvents e = case e of
  EventKey (SpecialKey KeySpace) Down (Modifiers Up Up Up) _ -> oneStep
  _ -> return

main :: IO ()
main = playIO
  (InWindow "gloss : Turtles!" (720,720) (0,0))
  black
  0
  initWorld
  renderWorld
  (const return)
  (const oneStep)

