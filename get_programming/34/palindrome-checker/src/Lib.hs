module Lib
  ( isPalindrome,
  )
where

import Data.Char (isPunctuation, isSpace)
import Data.Text as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)


preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanedText == T.reverse cleanedText where cleanedText = preprocess text
