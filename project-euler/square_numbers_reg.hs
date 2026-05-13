nums = [1 .. 442000]

oddNums = filter odd nums

sumOffSquares = sum (map (^ 2) oddNums)
