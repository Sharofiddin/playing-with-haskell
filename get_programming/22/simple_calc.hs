solve :: (Real a) => a -> Char -> a -> Double
solve fstArg '+' res = realToFrac res - realToFrac fstArg
solve fstArg '*' res = realToFrac res / realToFrac fstArg
