
import Data.Char

pigLatinWord :: String -> String
pigLatinWord word = case break isVowel word of
  ("",_)   -> word ++ "yay"
  (cs,rem) -> rem ++ cs ++ "ay"

isVowel :: Char -> Bool
isVowel = (`elem` "aeiouy") . toLower

pigLatin :: String -> String
pigLatin = unwords . map pigLatinWord . words

