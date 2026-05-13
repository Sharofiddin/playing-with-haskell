{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC

bcInt :: BC.ByteString
bcInt = "6"

bsToInt :: BC.ByteString -> Int
bsToInt bs = read (BC.unpack bs)
