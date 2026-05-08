toInts :: String -> [Int]
toInts input = map read (lines input)

main :: IO ()
main = do
  input <- getContents
  let nums = toInts input
  print (sum (map (^ 2) nums))
