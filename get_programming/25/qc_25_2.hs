import System.Random

randomChar :: IO Char
randomChar = do
  randInt <- randomRIO (0, 255)
  return (toEnum randInt)
