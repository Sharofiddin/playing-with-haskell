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
main = do
  putStrLn "What is the size of pizza 1"
  size1 <- getLine
  putStrLn "What is the cost of pizza 1"
  cost1 <- getLine
  putStrLn "What is the size of pizza 2"
  size2 <- getLine
  putStrLn "What is the cost of pizza 2"
  cost2 <- getLine
  let pizza1 = (read size1, read cost1)
  let pizza2 = (read size2, read cost2)
  putStrLn (describePizza (comparePizzas pizza1 pizza2))
