module Lib
  ( myTake,
    myTakePM,
    eitherHead,
    intExample,
    intExampleEmpty,
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
