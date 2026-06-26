module HaskellBrainTeasers.PoinlesslyFixed where

import Data.Function

main :: IO ()
main = print (go 12)
  where
    go = take <*> fix (((:) <*>) . (. succ))
