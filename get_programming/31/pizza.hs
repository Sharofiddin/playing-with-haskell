type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi * (diameter / 2) ^ 2

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 =
  if price1 < price2
    then p1
    else p2
  where
    price1 = costPerInch p1
    price2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) =
  "The "
    ++ show size
    ++ " pizza "
    ++ "is cheaper at "
    ++ show costSqInch
    ++ " per square inch"
  where
    costSqInch = costPerInch (size, cost)

main :: IO ()
main =
  putStrLn result
  where
    size1 = putStrLn "What is the size of pizza 1" >> getLine >>= read
    cost1 = putStrLn "What is the cost of pizza 1" >> getLine >>= read
    size2 = putStrLn "What is the size of pizza 2" >> getLine >>= read
    cost2 = putStrLn "What is the cost of pizza 2" >> getLine >>= read
    pizza1 = (\size cost -> return (size, cost)) size1 cost1
    pizza2 = (size2, cost2)

    comapred = pure comparePizzas <*> pizza1 <*> pizza2
