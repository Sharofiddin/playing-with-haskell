coffeeCups :: [Int]
coffeeCups = [6, 12]

consumedLastNight :: Int
consumedLastNight = 2

friends :: [Int]
friends = [2, 3]

longNightConsumption :: [Int]
longNightConsumption = [3, 4]

allLeft :: [Int]
allLeft = (\count -> count - 4) <$> coffeeCups

totalConsumers :: [Int]
totalConsumers = (+ 2) <$> friends

possibleConsumptions :: [Int]
possibleConsumptions = (*) <$> totalConsumers <*> longNightConsumption

possiblePurchaseCount :: [Int]
possiblePurchaseCount = (-) <$> possibleConsumptions <*> allLeft
