import Control.Arrow (ArrowChoice (left, right))
import Data.Map qualified as Map
import Data.Maybe

data RobotPart = RobotPart
  { name :: String,
    description :: String,
    cost :: Double,
    count :: Int
  }
  deriving (Show)

leftArm :: RobotPart
leftArm =
  RobotPart
    { name = "left arm",
      description = "left arm for face punching",
      cost = 1000.00,
      count = 3
    }

rightArm :: RobotPart
rightArm =
  RobotPart
    { name = "right arm",
      description = "right arm for kind hand gestures",
      cost = 1025.00,
      count = 5
    }

robotHead :: RobotPart
robotHead =
  RobotPart
    { name = "robot head",
      description = "this head looks mad",
      cost = 5092.25,
      count = 2
    }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part =
  mconcat
    [ "<h2",
      partName,
      "</h2>",
      "<p><h3>desc</h3>",
      partDesc,
      "</p><p><h3>cost</h3>",
      partCost,
      "</p><p><h3>count</h3>",
      partCount,
      "</p>"
    ]
  where
    partName = name part
    partDesc = description part
    partCost = show (cost part)
    partCount = show (count part)

partDB :: Map.Map Int RobotPart
partDB = Map.fromList keyVals
  where
    vals = [leftArm, rightArm, robotHead]
    keys = [1, 2, 3]
    keyVals = zip keys vals

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = map snd (Map.toList partDB)

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartDB :: Map.Map Int Html
htmlPartDB = renderHtml <$> partDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO String
htmlSnippet = renderHtml <$> leftArmIO

getLowerCost :: RobotPart -> RobotPart -> RobotPart
getLowerCost rp1 rp2 =
  if cost1 > cost2
    then rp2
    else rp1
  where
    cost1 = cost rp1
    cost2 = cost rp2

printPriceByID :: Maybe RobotPart -> String
printPriceByID Nothing = "Part not found"
printPriceByID (Just part) =
  mconcat
    [ name part,
      " costs ",
      show (cost part),
      " USD"
    ]

main :: IO ()
main = do
  putStrLn "Enter 1-ID"
  id1Str <- getLine
  let id1 = read id1Str
  putStrLn "Enter 2-ID"
  id2Str <- getLine
  let id2 = read id2Str
  let part1 = Map.lookup id1 partDB
  let part2 = Map.lookup id2 partDB
  let maybeLower = getLowerCost <$> part1 <*> part2
  putStrLn
    ( if isJust maybeLower
        then "lower cost is " ++ name (fromJust maybeLower) ++ " with price " ++ show (cost (fromJust maybeLower)) ++ " USD"
        else "Incorrect ID"
    )
