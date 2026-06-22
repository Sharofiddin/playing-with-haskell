module Main where

makeGreeting salutation person =
  salutation <> " " <> person

fizzBuzzFor num
  | 0 == num `rem` 15 = "fizzbuzz"
  | 0 == num `rem` 5 = "buzz"
  | 0 == num `rem` 3 = "fizz"
  | otherwise = show num

main = print $ makeGreeting "Hello" "George"
