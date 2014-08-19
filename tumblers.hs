
import Control.Arrow
import Control.Lens
import Control.Monad.State

type Puzzle a = State PState a

data Pin
  = Top
  | Middle
  | Bottom
  deriving (Eq,Ord,Show)

data PState = PState
  { _pins :: [Pin]
  , _tumblers :: [(Int,Tumbler)]
  }

makeLenses ''PState

pos = _1
tumbler = _2

type Tumbler = ([Bool],[Bool],[Bool])

up :: Int -> Maybe Puzzle
up n = do
  p <- use pins

