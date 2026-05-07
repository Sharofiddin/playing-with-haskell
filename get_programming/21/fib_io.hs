fastFib :: Integer -> Integer -> Integer -> Integer
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y z = fastFib (x + y) x (z - 1)

showFib :: Integer -> String
showFib n =
  "Fib of "
    ++ show n
    ++ " is "
    ++ fib
  where
    fib = show (fastFib 1 1 n)

main :: IO ()
main = do
  putStrLn "Enter num"
  num <- getLine
  putStrLn (showFib (read num))
