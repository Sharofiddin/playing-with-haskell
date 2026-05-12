import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as TIO

toInts :: T.Text -> [Int]
toInts = map (read . T.unpack) . T.lines

main :: IO ()
main = do
  input <- TIO.getContents
  let numbers = toInts input
  TIO.putStrLn ((T.pack . show . sum) numbers)
