import System.Environment (getArgs)

getCounts :: String -> (Int, Int, Int)
getCounts txt = (charCount, wordCount, lineCount)
  where
    charCount = length txt
    wordCount = (length . words) txt
    lineCount = (length . lines) txt

countsText :: (Int, Int, Int) -> String
countsText (cc, wc, lc) =
  unwords
    [ "chars:",
      show cc,
      "words:",
      show wc,
      "lines:",
      show lc
    ]

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  content <- readFile fileName
  let summary = countsText (getCounts content)
  appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])
  putStrLn summary
