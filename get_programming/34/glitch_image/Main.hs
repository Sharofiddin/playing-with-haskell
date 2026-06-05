module Main where

import Control.Monad (foldM)
import Data.ByteString.Char8 qualified as BC
import GlitchImage
import System.Environment

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
