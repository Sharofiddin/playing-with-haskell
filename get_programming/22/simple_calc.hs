import Data.List.Split
import GHC.Float (formatRealFloat)

solve :: (Real a) => a -> Char -> a -> Double
solve fstArg '+' res = realToFrac res - realToFrac fstArg
solve fstArg '*' res = realToFrac res / realToFrac fstArg

solveStr :: String -> Double
solveStr equation = solve firstArg (head operation) res
  where
    equationSides = splitOn "=" equation
    resStr = last equationSides
    res = read resStr
    operation =
      if '+' `elem` head equationSides
        then "+"
        else "*"
    args = splitOn operation (head equationSides)
    firstArg =
      if head args == "x"
        then read (last args)
        else read (head args)

main :: IO ()
main = do
  input <- getContents

  let solution = map solveStr (lines input)
  print solution
