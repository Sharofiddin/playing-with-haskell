myGCD a b = case remainder of 
     0 -> b
     _ -> myGCD b remainder
    where remainder = a `mod` b
