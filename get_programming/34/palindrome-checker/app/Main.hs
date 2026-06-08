{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text as T
import Data.Text.IO as TIO
import Lib

main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if its palindorme"
  text <- TIO.getLine
  let response =
        if isPalindrome text
          then "It is!"
          else "It isn't!"
  TIO.putStrLn response
