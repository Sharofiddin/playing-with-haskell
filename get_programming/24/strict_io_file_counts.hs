{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)

getCounts :: T.Text -> (Int, Int, Int)
getCounts text = (cc, wc, lc)
  where
    cc = T.length text
    wc = length (T.words text)
    lc = length (T.lines text)

countsText :: (Int, Int, Int) -> T.Text
countsText (cc, wc, lc) =
  T.pack
    ( unwords
        [ "chars:",
          show cc,
          "words:",
          show wc,
          "lines:",
          show lc
        ]
    )

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  content <- TIO.readFile fileName
  let summary = countsText (getCounts content)
  TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
  TIO.putStrLn summary
