{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Turtle where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector

import Control.Applicative
import Control.Monad.Writer
import Control.Monad.State
import Data.IORef

-- TurtleM {{{

newtype TurtleM a = TurtleM
  { unTurtleM :: StateT [TurtleRef] (WriterT Picture IO) a }
  deriving
    ( Functor
    , Monad
    , Applicative
    , MonadIO
    , MonadWriter Picture
    , MonadState [TurtleRef]
    )

runTurtleM :: [TurtleRef] -> TurtleM a -> IO (a,Picture,[TurtleRef])
runTurtleM ts m = do
  ((a,ts'),p) <- runWriterT $ flip runStateT ts $ unTurtleM m
  return (a,p,ts')

addTurtle :: TurtleRef -> TurtleM ()
addTurtle t = modify (t :)

readTurtle :: TurtleRef -> TurtleM Turtle
readTurtle = liftIO . readIORef

getTurtles :: TurtleM [TurtleRef]
getTurtles = get

modifyTurtle :: (Turtle -> Turtle) -> TurtleRef -> TurtleM ()
modifyTurtle f r = liftIO $ modifyIORef' r f

-- }}}

-- Turtle {{{

data Turtle = Turtle
  { turtlePos   :: Point
  , turtleAngle :: Angle
  , turtleColor :: Color
  , penIsDown     :: Bool
  }

type TurtleRef = IORef Turtle

newTurtle :: Turtle
          -> TurtleM TurtleRef
newTurtle t = do
  r <- liftIO $ newIORef t
  addTurtle r
  return r

newDefTurtle :: TurtleM TurtleRef
newDefTurtle = newTurtle defTurtle

newTurtleWithColor :: Color -> TurtleM TurtleRef
newTurtleWithColor c = newTurtle $ defTurtle { turtleColor = c }

defTurtle :: Turtle
defTurtle = Turtle
  { turtlePos   = (0,0)
  , turtleAngle = 0
  , turtleColor = white
  , penIsDown     = True
  }

-- }}}

-- Angle {{{

newtype Angle = Angle { fromAngle :: Float } deriving (Show)

instance Num Angle where
  fromInteger             = mkAngle . fromInteger
  (Angle a1) + (Angle a2) = mkAngle $ a1 + a2
  (Angle a1) - (Angle a2) = mkAngle $ a1 - a2
  (Angle a1) * (Angle a2) = mkAngle $ a1 * a2
  abs (Angle a1)          = mkAngle $ abs a1
  signum (Angle a1)       = mkAngle $ signum a1

mkAngle :: Float -> Angle
mkAngle theta
  | theta >= 360 = mkAngle $ theta - 360
  | theta <  0   = mkAngle $ theta + 360
  | otherwise    = Angle theta

-- }}}

-- Turtle Actions {{{

allTurtles :: [TurtleRef -> TurtleM ()] -> TurtleM ()
allTurtles fs = do
  ts <- getTurtles
  turtlesDo ts fs

onTurtle :: (Turtle -> Turtle) -> TurtleRef -> TurtleM (Turtle,Turtle)
onTurtle f r = do
  t1 <- readTurtle r
  modifyTurtle f r
  t2 <- readTurtle r
  return (t1,t2)

onTurtle_ :: (Turtle -> Turtle) -> TurtleRef -> TurtleM ()
onTurtle_ f r = do
  _ <- onTurtle f r
  return ()

turtlesDo :: [TurtleRef] -> [TurtleRef -> TurtleM ()] -> TurtleM ()
turtlesDo ts fs = sequence_ [ sequence_ $ map ($ t) fs | t <- ts ]

drawLine :: (Turtle -> Turtle) -> TurtleRef -> TurtleM ()
drawLine f r = do
  (t1,t2) <- onTurtle f r
  let pos1 = turtlePos t1
  let pos2 = turtlePos t2
  when (penIsDown t1 && (pos1 /= pos2)) $
    tell $ color (turtleColor t1) $ line [pos1,pos2]

setColor :: Color -> TurtleRef -> TurtleM ()
setColor c = onTurtle_ $ \t -> t { turtleColor = c }

penUp :: TurtleRef -> TurtleM ()
penUp = onTurtle_ $ \t -> t { penIsDown = False }

penDown :: TurtleRef -> TurtleM ()
penDown = onTurtle_ $ \t -> t { penIsDown = True }

togglePen :: TurtleRef -> TurtleM ()
togglePen = onTurtle_ $ \t -> t { penIsDown = not $ penIsDown t }

left :: Angle -> TurtleRef -> TurtleM ()
left theta = onTurtle_ $ \t -> t { turtleAngle = turtleAngle t + theta }

right :: Angle -> TurtleRef -> TurtleM ()
right theta = onTurtle_ $ \t -> t { turtleAngle = turtleAngle t - theta }

forward :: Float -> TurtleRef -> TurtleM ()
forward delta = drawLine $ \t ->
  let pos = newPos delta (turtleAngle t) (turtlePos t)
    in t { turtlePos = pos }

backward :: Float -> TurtleRef -> TurtleM ()
backward delta = drawLine $ \t ->
  let pos = newPos (-delta) (turtleAngle t) (turtlePos t)
    in t { turtlePos = pos }

data ArcDir
  = ToLeft
  | ToRight

drawArc :: ArcDir -> Angle -> Float -> TurtleRef -> TurtleM ()
drawArc dir theta radius r = do
  let f t =  t { turtlePos = finalTurPos t , turtleAngle = finalTurAngle t }
  (t1,_) <- onTurtle f r
  let (x,y) = focusPos t1
  let a1 = fromAngle $ mkAngle $ fromAngle $ initOrigAngle t1
  let a2 = fromAngle $ mkAngle $ fromAngle $ finalOrigAngle t1
  when (penIsDown t1) $
    tell $ color (turtleColor t1)
         $ translate x y
         $ case dir of
             ToLeft  -> arc a1 a2 radius
             ToRight -> arc a2 a1 radius
  where
  initOrigAngle  t = 
                     (case dir of
                       ToLeft  -> (-)
                       ToRight -> (+))
                       (turtleAngle t)
                       90
  finalOrigAngle t = (case dir of
                       ToLeft  -> (+)
                       ToRight -> (-))
                       (initOrigAngle t)
                       theta
  focusPos       t = findFocus dir radius t
  finalTurAngle  t = (case dir of
                       ToLeft  -> (+)
                       ToRight -> (-))
                       (turtleAngle t)
                       theta
  finalTurPos    t = (radius `mulSV`
                       unitVectorAtAngle
                         (degs2rads $ finalOrigAngle t))
                       + focusPos t
  
newPos :: Float -> Angle -> Point -> Point
newPos delta theta pos = (delta `mulSV` v) + pos
  where
  v   = unitVectorAtAngle $ degs2rads theta

findFocus :: ArcDir -> Float -> Turtle -> Point
findFocus arcDir radius t = newPos radius dir pos
  where
  pos = turtlePos t
  dir = (case arcDir of
          ToLeft  -> (+)
          ToRight -> (-))
          (turtleAngle t)
          90

degs2rads :: Angle -> Float
degs2rads (Angle theta) = pi * theta / 180

stamp :: String -> TurtleRef -> TurtleM ()
stamp msg r = do
  t     <- readTurtle r
  let (x,y) = turtlePos   t
  let c     = turtleColor t
  tell $ color c $ translate x y $ scale 0.1 0.1 $ text msg

-- }}}

