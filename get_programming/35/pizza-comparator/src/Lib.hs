module Lib (
  comparePizzas,
  describePizza,
)
where

type Pizza = (Double, Double)

areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi * (diameter / 2) * (diameter / 2)

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
