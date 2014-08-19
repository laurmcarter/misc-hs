{-# LANGUAGE OverloadedStrings #-}

import Data.String

test :: Int -> Bool -> String
test x y = show x ++ show y

instance IsString Int where
  fromString = read

instance IsString Bool where
  fromString = read

x = test "123" "True"

