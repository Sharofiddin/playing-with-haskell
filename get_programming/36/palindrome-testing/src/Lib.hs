module Lib (
  isPalindrome,
  preprocess,
)
where

import Data.Char (isPunctuation, isSpace)
import Data.Text as T

removeWhiteSpace :: T.Text -> T.Text
removeWhiteSpace = T.filter (not . isSpace)

removePunctuation :: T.Text -> T.Text
removePunctuation = T.filter (not . isPunctuation)

preprocess :: T.Text -> T.Text
preprocess = removePunctuation . removeWhiteSpace . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanedText == T.reverse cleanedText
 where
  cleanedText = preprocess text
