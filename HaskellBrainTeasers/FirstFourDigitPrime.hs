module HaskellBrainTeasers.FirstFourDigitPrime where

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve [] = []
    sieve (prime : candidates) =
      prime : sieve (filter divisible candidates)
      where
        divisible candidate = candidate `rem` prime /= 0

firstBigNumer :: [Int] -> Maybe Int
firstBigNumer =
  foldr findFirst Nothing
  where
    findFirst someNum isFound
      | length (show someNum) >= 4 = Just someNum
      | otherwise = isFound

main :: IO ()
main =
  case firstBigNumer primes of
    Nothing -> putStrLn "no big primes"
    Just aPrime -> putStrLn ("the first big prime is " ++ show aPrime)
