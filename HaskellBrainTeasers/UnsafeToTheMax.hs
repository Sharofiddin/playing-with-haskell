module HaskellBrainTeasers.UnsafeToTheMax where

import Control.Monad
import Data.Foldable
import Data.IORef
import System.IO.Unsafe

unsafeMax :: [Int] -> IO Int
unsafeMax vals = do
    for_ vals $ \val -> do
        currentMax <- readIORef maxRef
        when (val > currentMax) $
            writeIORef maxRef val
    readIORef maxRef
  where
    maxRef = unsafePerformIO $ newIORef 0

main :: IO ()
main = do
    four <- unsafeMax [4, 3, 2]
    three <- unsafeMax [3, 2, 1]
    two <- unsafeMax [2, 1, 0]
    print [four, three, two]
