toInts :: String -> [Int]
toInts userInput = map read (lines userInput)

main :: IO ()
main = do
  userInput <- getContents
  let nums = toInts userInput
  print (sum nums)
