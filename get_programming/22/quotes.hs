quoteRet :: String -> String
quoteRet "1" = "quote1"
quoteRet "2" = "quote2"
quoteRet "3" = "quote3"
quoteRet "4" = "quote4"
quoteRet "5" = "quote5"
quoteRet _ = "No quote"

quoteProc :: [String] -> [String]
quoteProc ("n" : xs) = []
quoteProc (x : xs) = quoteRet x : quoteProc xs

main :: IO ()
main = do
  putStrLn "Enter number between 1 and 5, or n to exit"
  input <- getContents
  let quotes = quoteProc (lines input)
  mapM_ print quotes
