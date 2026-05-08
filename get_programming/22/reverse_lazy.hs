main :: IO ()
main = do
  content <- getContents
  let reversed = reverse content
  putStrLn reversed
