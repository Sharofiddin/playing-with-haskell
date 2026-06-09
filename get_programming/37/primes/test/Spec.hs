import Data.Maybe (fromJust, isJust, isNothing)
import Primes
import Test.QuickCheck

prop_validPrimesOnly val =
  if val < 2 || val >= length primes
    then isNothing result
    else isJust result
 where
  result = isPrime val

prop_primesArePrime val =
  if result == Just True
    then length divisors == 0
    else True
 where
  result = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_compositesAreComposite val =
  if result == Just False
    then length divisors > 0
    else True
 where
  result = isPrime val
  divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal n =
  if result == Nothing
    then True
    else product (fromJust result) == n
 where
  result = primeFactors n

prop_allFactorsPrime n =
  if isNothing result
    then True
    else all (== Just True) resultsPrime
 where
  result = primeFactors n
  resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheck prop_primesArePrime
  quickCheck prop_compositesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
