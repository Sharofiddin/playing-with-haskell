import Data.Char (isPunctuation, isSpace)
import Data.Text as T
import Lib (isPalindrome, preprocess)
import Test.QuickCheck
import Test.QuickCheck.Instances

prop_punctuationInvariant text = preprocess text == preprocess noPunctText
 where
  noPunctText = T.filter (not . isPunctuation) text

prop_whitespaceInvariant text = preprocess text == preprocess noPunctText
 where
  noPunctText = T.filter (not . isSpace) text

prop_lettercaseInvariant text = preprocess text == preprocess (T.toLower text)

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

main :: IO ()
main = do
  putStrLn "Running tests ..."
  quickCheckWith stdArgs{maxSuccess = 1000} prop_punctuationInvariant
  quickCheckWith stdArgs{maxSuccess = 1000} prop_lettercaseInvariant
  quickCheckWith stdArgs{maxSuccess = 1000} prop_whitespaceInvariant
  quickCheck prop_reverseInvariant
  putStrLn "done"
