{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let srcFileName = head args
  let trgtFileName = last args
  content <- TIO.readFile srcFileName
  TIO.writeFile trgtFileName content
  TIO.putStrLn "Done"
