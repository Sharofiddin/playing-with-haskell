import Data.Text qualified as T
import Data.Text.IO as TIO
import System.Environment

capitalize :: T.Text -> T.Text
capitalize word =
  mconcat [T.pack [first], T.tail word]
  where
    first =
      if T.head word `T.elem` T.pack ['a' .. 'z']
        then toEnum (fromEnum (T.head word) - 32)
        else T.head word

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  content <- TIO.readFile fileName
  let capitalizedContent = T.unwords (map capitalize (T.words content))
  TIO.writeFile fileName capitalizedContent
