{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T

tLines :: T.Text -> [T.Text]
tLines = T.splitOn "\n"

tUnline :: [T.Text] -> T.Text
tUnline = T.intercalate "\n"

text :: T.Text
text = "Salom\nqalaysan"
