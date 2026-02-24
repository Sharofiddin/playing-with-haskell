inFirstHalf e ls = elem e fstHalf
  where len = length ls 
        mid = len `div` 2 
        fstHalf = take mid ls
  
