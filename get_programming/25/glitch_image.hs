import Control.Monad (foldM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import System.Environment
import System.Random (randomRIO)

intToChar :: Int -> Char
intToChar num = toEnum safeNum
  where
    safeNum = num `mod` 255

intToBC :: Int -> BC.ByteString
intToBC num = BC.pack [intToChar num]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc num bytes =
  mconcat [before, replacement, after]
  where
    (before, rest) = BC.splitAt loc bytes
    after = BC.drop 1 rest
    replacement = intToBC num

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  charVal <- randomRIO (0, 255)
  return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  let rndBound = bytesLength - sectionSize
  start <- randomRIO (0, rndBound)
  return (sortSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSection,
    randomReplaceByte,
    randomSortSection,
    randomReplaceByte
  ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  imageFile <- BC.readFile fileName
  glitched <-
    foldM
      (\bytes func -> func bytes)
      imageFile
      glitchActions
  let glitchedName = mconcat ["glitched_", fileName]
  BC.writeFile glitchedName glitched
  print "All done"
