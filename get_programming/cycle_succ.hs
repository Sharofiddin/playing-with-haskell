cycleSucc :: (Bounded a, Enum a, Eq a ) => a -> a
cycleSucc n = if maxBound == n 
              then minBound 
              else succ n
