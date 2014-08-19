
module Remind where

data REM = REM
  { once :: Bool
  , dateSpec :: DateSpec
  , priority :: Maybe Priority
  , skip     :: Maybe Skip
  , timed    :: Maybe TimeSpec
  , expire   :: Maybe Expiry
  , token    :: Token
  } deriving (Eq,Show)

data Token
  = Msg Body
  | Msf Body
  | Run Body
  | Cal Body
  deriving (Eq,Show)
 
data TimeSpec = TimeSpec
  { time :: Time
  , tdelta :: Maybe Minutes
  , trepeat :: Maybe Minutes
  , tduration :: Maybe Minutes
  } deriving (Eq,Show)

data Time = Time
  { hour :: Hours
  , mins :: Minutes
  } deriving (Eq,Show)

newtype Hours = Hours
  { getHours :: Int
  } deriving (Eq,Show)

newtype Minutes = Minutes
  { getMinutes :: Int
  } deriving (Eq,Show)

newtype Days = Days
  { getDays :: Int
  } deriving (Eq,Show)

newtype OmitList = OmitList
  { omitWeekdays :: [Weekday]
  } deriving (Eq,Show)

data DateSpec = DateSpec
  { dates    :: Dates
  , ddelta    :: Maybe Advance
  , dback     :: Maybe BackScan
  , drepeat   :: Maybe Repeat
  } deriving (Eq,Show)

data Dates = Dates
  { days     :: Restrict Day
  , months   :: Restrict Month
  , years    :: Restrict Year
  , weekdays :: Restrict Weekday
  } deriving (Eq,Show)

data Restrict a
  = All
  | Only [a]
  deriving (Eq,Show)

everyday :: DateSpec
everyday = DateSpec
  { dates = Dates
    { days = All
    , months = All
    , years = All
    , weekdays = All 
    }
  , ddelta = Nothing
  , dback = Nothing
  , drepeat = Nothing
  }

onDays :: [Day] -> DateSpec
onDays ds = everyday { dates = (dates everyday) { days = Only ds } }

duringMonths :: [Month] -> DateSpec
duringMonths ms = everyday { dates = (dates everyday) { months = Only ms } }

duringYears :: [Year] -> DateSpec
duringYears ys = everyday { dates = (dates everyday) { years = Only ys } }

daysInMonths :: [Day] -> [Month] -> DateSpec
daysInMonths ds ms = everyday
  { dates =
    (dates everyday)
      { days = Only ds
      , months = Only ms
      }
  }

daysInYears :: [Day] -> [Year] -> DateSpec
daysInYears ds ys = everyday
  { dates =
    (dates everyday)
      { days = Only ds
      , years = Only ys
      }
  }

specificDates :: [Day] -> [Month] -> [Year] -> DateSpec
specificDates ds ms ys = everyday
  { dates =
    (dates everyday)
      { days = Only ds
      , months = Only ms
      , years = Only ys
      }
  }

daysOfWeek :: [Weekday] -> DateSpec
daysOfWeek ws = everyday { dates = (dates everyday) { weekdays = Only ws } }

weekdaysAfter :: [Weekday] -> Day -> DateSpec
weekdaysAfter ws d = everyday
  { dates =
    (dates everyday)
      { days = Only [d]
      , weekdays = Only ws
      }
  }

weekdaysInMonths :: [Weekday] -> [Month] -> DateSpec
weekdaysInMonths ws ms = everyday
  { dates =
    (dates everyday)
      { months = Only ms
      , weekdays = Only ws
      }
  }

weekdaysInYears :: [Weekday] -> [Year] -> DateSpec
weekdaysInYears ws ys = everyday
  { dates =
    (dates everyday)
      { years = Only ys
      , weekdays = Only ws
      }
  }

weekdaysAfterInMonths :: [Weekday] -> Day -> [Month] -> DateSpec
weekdaysAfterInMonths ws d ms = everyday
  { dates =
    (dates everyday)
      { days = Only [d]
      , months = Only ms
      , weekdays = Only ws
      }
  }

weekdaysAfterInYears :: [Weekday] -> Day -> [Year] -> DateSpec
weekdaysAfterInYears ws d ys = everyday
  { dates =
    (dates everyday)
      { days = Only [d]
      , years = Only ys
      , weekdays = Only ws
      }
  }

weekdaysInMonthsAndYears :: [Weekday] -> [Month] -> [Year] -> DateSpec
weekdaysInMonthsAndYears ws ms ys = everyday
  { dates =
    (dates everyday)
      { months = Only ms
      , years = Only ys
      , weekdays = Only ws
      }
  }

weekdaysAfterInMonthsAndYears :: [Weekday] -> Day -> [Month] -> [Year] -> DateSpec
weekdaysAfterInMonthsAndYears ws d ms ys = everyday
  { dates =
    (dates everyday)
      { days = Only [d]
      , months = Only ms
      , years = Only ys
      , weekdays = Only ws
      }
  }

newtype Priority = Priority
  { getPriority :: Int
  } deriving (Eq,Show)

data Skip
  = SKIP
  | BEFORE
  | AFTER
  deriving (Eq,Show)

data Expiry = Expiry
  { exprDay :: Day
  , exprMonth :: Month
  , exprYear :: Year
  } deriving (Eq,Show)

data Advance
  = AdvanceCount Days OmitList
  | AdvanceNoCount Days OmitList
  deriving (Eq,Show)

data BackScan
  = BackScanCount Days OmitList
  | BackScanNoCount Days OmitList
  deriving (Eq,Show)

newtype Repeat = Repeat
  { getRepeat :: Days
  } deriving (Eq,Show)

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq,Ord,Bounded,Show)

data Weekday
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq,Ord,Bounded,Show)

newtype Day   = D Int deriving (Eq,Ord,Show)
newtype Year  = Y Int deriving (Eq,Ord,Show)

daysInMonth :: Year -> Month -> Int
daysInMonth y m = case m of
  January   -> 31
  February  -> gregorianLeap y
  March     -> 31
  April     -> 30
  May       -> 31
  June      -> 30
  July      -> 31
  August    -> 31
  September -> 30
  October   -> 31
  November  -> 30
  December  -> 31
  
gregorianLeap :: Year -> Int
gregorianLeap (Y y)
  | not (4 `isDiv` y)                    = 28
  | 100 `isDiv` y && not (400 `isDiv` y) = 28
  | otherwise                            = 29

isDiv :: Integral a => a -> a -> Bool
isDiv y x = (x `mod` y) == 0

data Body = Body
  { getBody :: [Comp]
  } deriving (Eq,Show)

data Comp
  = Lit String
  | Subst Char
  deriving (Eq,Show)

