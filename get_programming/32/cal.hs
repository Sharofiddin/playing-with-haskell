getDayInMonth :: Int -> Int
getDayInMonth 1 = 31
getDayInMonth 2 = 28
getDayInMonth 3 = 31
getDayInMonth 4 = 30
getDayInMonth 5 = 31
getDayInMonth 6 = 30
getDayInMonth 7 = 31
getDayInMonth 8 = 31
getDayInMonth 9 = 30
getDayInMonth 10 = 31
getDayInMonth 11 = 30
getDayInMonth 12 = 31

calendar :: [Int]
calendar =
  [ days | months <- [1 .. 12], let daysMax = getDayInMonth months, days <- [1 .. daysMax]
    -- let days = [1..daysMax] each month its own array
  ]

monthEnds :: [Int]
monthEnds = [31,28,31,30,31,30,31,31,30,31,30,31]

dates :: [Int] -> [Int]
dates ends = [date| end <- ends, date <- [1 ..end ] ]
