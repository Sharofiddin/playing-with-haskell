fib :: Int -> Int -> Int -> Int
fib _ _ 0 = 0
fib _ _ 1 = 1
fib _ _ 2 = 1
fib x y 3 = x + y
fib x y n = fib (x + y) x (n - 1)

solution :: Int
solution = sum $ filter even (takeWhile (< 4000000) fibList)
  where
    fibList = map (fib 1 1) [1 ..]
