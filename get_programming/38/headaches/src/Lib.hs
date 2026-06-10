module Lib
  ( myTake,
    myTakePM,
    eitherHead,
    intExample,
    intExampleEmpty,
    sieve,
    isPrime,
    displayResult,
  )
where

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = head xs : myTake (n - 1) (tail xs)

myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x : xs) = x : myTakePM (n - 1) xs

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head becuase the list is empty"
eitherHead (x : xs) = Right x

intExample :: [Int]
intExample = [1, 2, 3]

intExampleEmpty :: [Int]
intExampleEmpty = []

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x : xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)

maxN :: Int
maxN = 10000

primes :: [Int]
primes = sieve [2 .. maxN]

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
  show TooLarge = "Value exceed max bound"
  show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
  | n < 2 = Left InvalidValue
  | n >= maxN = Left TooLarge
  | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It'prime!"
displayResult (Right False) = "It isn't prime!"
displayResult (Left primeError) = show primeError
