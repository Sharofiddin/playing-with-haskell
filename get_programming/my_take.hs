 myTake 0 xs = []                        
 myTake _ [] = []
 myTake n (x:xs) = x : myTake (n-1) xs
