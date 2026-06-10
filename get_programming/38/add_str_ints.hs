import Data.Char (isDigit)

data AdderError = FirstValInvalid | SecondValInvalid | BothInvalid

instance Show AdderError where
  show FirstValInvalid = "First value can't be parsed"
  show SecondValInvalid = "Second value can't be parsed"
  show BothInvalid = "Neither values can be parsed"

isStrNum :: String -> Bool
isStrNum = all isDigit

addStrInts :: String -> String -> Either AdderError Int
addStrInts s1 s2
  | not (isStrNum s1) && not (isStrNum s2) = Left BothInvalid
  | not (isStrNum s1) = Left FirstValInvalid
  | not (isStrNum s2) = Left SecondValInvalid
  | otherwise = Right (read s1 + read s2)

displayResult :: Either AdderError Int -> String
displayResult (Right sum) = "Sum is " ++ show sum
displayResult (Left error) = show error

main :: IO ()
main = do
  putStrLn "Enter first value:"
  s1 <- getLine
  putStrLn "Enter second value:"
  s2 <- getLine
  let result = addStrInts s1 s2
  putStrLn (displayResult result)
