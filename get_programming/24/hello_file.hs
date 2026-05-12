import System.IO

main :: IO ()
main = do
  helloFile <- openFile "24/hello.txt" ReadMode
  firstLine <- hGetLine helloFile
  putStrLn firstLine
  secondLine <- hGetLine helloFile
  goodByFile <- openFile "goodbye.txt" WriteMode
  hPutStrLn goodByFile secondLine
  hClose helloFile
  hClose goodByFile
  putStrLn "done"

