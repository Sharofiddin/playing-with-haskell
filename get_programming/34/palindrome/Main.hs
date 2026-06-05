{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO qualified as TIO
import Palindrome

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's palindorme!"
  text <- TIO.getLine
  let response =
        if isPalindorme text
          then "it is!"
          else "it's not"
  print response
