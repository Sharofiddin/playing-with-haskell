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
main =
  res >>= putStrLn
  where
    size1 = putStrLn "What is the size of pizza 1" >> getLine >>= (return . read)
    cost1 = putStrLn "What is the cost of pizza 1" >> getLine >>= (return . read)
    size2 = putStrLn "What is the size of pizza 2" >> getLine >>= (return . read)
    cost2 = putStrLn "What is the cost of pizza 2" >> getLine >>= (return . read)
    pizza1 = (\size cost -> (size, cost)) <$> size1 <*> cost1
    pizza2 = (\size cost -> (size, cost)) <$> size2 <*> cost2
    compared = comparePizzas <$> pizza1 <*> pizza2
    res = describePizza <$> compared

size1List :: [Double]
size1List = [10.0, 20.0, 30.0, 40.0]

cost1List :: [Double]
cost1List = [15.0, 25.0, 35.0, 45.0]

size2List :: [Double]
size2List = [200.0, 300.0, 400.0, 500.0]

cost2List :: [Double]
cost2List = [150.0, 250.0, 350.0, 450.0]

mainList :: [String]
mainList = do
  size1 <- size1List
  cost1 <- cost1List
  size2 <- size2List
  cost2 <- cost2List
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)

universalMain ::
  (Monad m) =>
  m Double ->
  m Double ->
  m Double ->
  m Double ->
  m String
universalMain s1 c1 s2 c2 = do
  size1 <- s1
  cost1 <- c1
  size2 <- s2
  cost2 <- c2
  let pizza1 = (size1, cost1)
  let pizza2 = (size2, cost2)
  let betterPizza = comparePizzas pizza1 pizza2
  return (describePizza betterPizza)
