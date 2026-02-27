collatz 1 = 1
collatz n = if reminder == 0 then 1 + collatz (n `div` 2) else 1 + collatz (n * 3 + 1)  
    where reminder = n `mod` 2
