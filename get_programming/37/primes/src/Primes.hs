module Primes where

primes :: [Int]
primes = sieve [2 .. 10000]

maxPrime :: Int
maxPrime = last primes

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
 where
  noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n
  | n < 2 = Nothing
  | n > last primes = Nothing
  | otherwise = Just (n `elem` primes)

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 [] = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next : restPrimes) =
  if n `mod` next == 0
    then next : unsafePrimeFactors (n `div` next) (next : restPrimes)
    else unsafePrimeFactors n restPrimes

primeFactors :: Int -> Maybe [Int]
primeFactors n
  | n < 2 = Nothing
  | n > last primes = Nothing
  | otherwise = Just (unsafePrimeFactors n primesLessThanN)
 where
  primesLessThanN = filter (<= n) primes
