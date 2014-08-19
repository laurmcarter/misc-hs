
import Control.Monad.Writer
import Data.Monoid
import Control.Lens

-- Data {{{

data Project = Project
  { _projectName :: String
  , _projectStartDate :: Date
  , _projectEndDate :: Maybe Date    
  , _projectTasks :: [Task]
  } deriving (Eq,Show)

data Date = Date
  { _day :: Day
  , _month :: Month
  , _year :: Year
  } deriving (Eq,Show)

data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Eq,Show)

type Day  = Int
type Year = Int
newtype Task = Task
  { taskDesc :: String
  } deriving (Eq,Show)

makeLenses ''Project
makeLenses ''Date

-- }}}

withTask :: Task -> Project -> Project
withTask t = projectTasks %~ (|> t)

project :: String -> Date -> Writer (Endo Project) Task -> Project
project name start builder = appEndo (execWriter builder)
  Project
    { _projectName = name
    , _projectStartDate = start
    , _projectEndDate = Nothing
    , _projectTasks = mempty
    }


task :: String -> Writer (Endo Project) Task
task name = do
  tell $ Endo $ withTask t
  return t
  where
  t = Task name

