{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module ActorFinal where

-- | 2D position on the screen.
type Position   = (Float, Float)

-- | Force and velocity vectors.
type Force  = (Float, Float)
type Velocity   = (Float, Float)

-- | Time in seconds
type Time   = Float

-- | Radius of a bead
type Radius = Float

-- | Each actor has its own unique index.
type Index  = Int

class Wall r where
  wall :: Index -> Position -> Position -> r

class Bead r where
  bead :: Index -> Int -> Radius -> Position -> Velocity -> r

instance Wall Index where
  wall i _ _ = i

instance Bead Index where
  bead i _ _ _ _ = i

newtype SetIndex r = SetIndex
  { setIndex :: Index -> r
  }

instance Wall r => Wall (SetIndex r) where
  wall _ p1 p2 = SetIndex $ \i -> wall i p1 p2

instance Bead r => Bead (SetIndex r) where
  bead _ s r p v = SetIndex $ \i -> bead i s r p v

newtype SetMode r = SetMode
  { setMode :: Int -> r
  }

instance Bead r => Bead (SetMode r) where
  bead ix _ r p v = SetMode $ \m -> bead ix m r p v

{-
-- | Set whether a bead is stuck
actorSetMode :: Int -> Actor -> Actor
actorSetMode m (Bead ix _ r p v)
    = Bead ix m r p v

-- | The actors in the world.
data Actor
    = Wall  !Index      -- ^ unique index of this actor
        !Position   -- ^ wall starting point
        !Position   -- ^ wall ending point

    | Bead  !Index      -- ^ unique index of this actor 
        !Int        -- ^ whether the bead is stuck
        !Radius     -- ^ radius of bead
        !Position   -- ^ position of bead
        !Velocity   -- ^ velocity of bead

    deriving Show

-- | Equality and ordering of actors will consider their index only.
--  We need Ord so we can put them in Maps and Sets.
instance Eq Actor where
 a1 == a2   = actorIx a1 == actorIx a2
    
instance Ord Actor where
 compare a1 a2  = compare (actorIx a1) (actorIx a2)

-- | Check whether an actor is a bead.
isBead :: Actor -> Bool
isBead (Bead {})    = True
isBead _        = False


-- | Check whether an actor is a wall.
isWall :: Actor -> Bool
isWall (Wall {}) = True
isWall _         = False


-- | Take the index of an actor
actorIx :: Actor -> Index
actorIx actor
 = case actor of
    Wall ix _ _ -> ix
    Bead ix _ _ _ _ -> ix


-- | Set the index of an actor
actorSetIndex :: Actor -> Index -> Actor
actorSetIndex actor ix
 = case actor of
    Bead _ m r pos vel  -> Bead ix m r pos vel 
    Wall _ p1 p2        -> Wall ix p1 p2


-}

