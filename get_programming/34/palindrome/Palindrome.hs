module Palindrome (isPalindorme) where

import Data.Char (isPunctuation, isSpace, toLower)
import Data.Text qualified as T

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.toLower

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindorme :: T.Text -> Bool
isPalindorme text = cleaned == T.reverse cleaned
  where
    cleaned = preprocess text
