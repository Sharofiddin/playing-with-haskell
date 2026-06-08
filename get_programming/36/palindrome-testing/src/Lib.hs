module Lib
  ( isPalindrome,
  )
where

isPalindrome :: String -> Bool
isPalindrome text = cleanedText == reverse cleanedText
  where
    cleanedText = filter (not . (`elem` ['!', '.'])) text
