
import Remind

r1 = REM
  { once = False
  , dateSpec = everyday
  , priority = Nothing
  , skip = Nothing
  , timed = Just $
    TimeSpec
      { time = Time (Hours 12) (Minutes 00)
      , tdelta = Nothing
      , trepeat = Nothing
      , tduration = Nothing
      }
  , expire = Nothing
  , token = Msg $
    Body $
      [ Lit "It is noon on "
      , Subst 'a'
      ]
  }
