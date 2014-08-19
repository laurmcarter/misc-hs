{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod

data Foo = Foo

instance Yesod Foo

mkYesod "Foo" [parseRoutes|
  / HomeR GET
|]

getHomeR = defaultLayout [whamlet|
  Welcome to the Foosty!
  |]

main = warpDebug 3000 Foo

