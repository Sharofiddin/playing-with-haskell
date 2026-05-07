import Data.Map qualified as Map

helloPerson :: String -> String
helloPerson name = "Hello " ++ name

userInput :: Map.Map String String
userInput = Map.fromList [("name", "Zero")]

mainMaybe :: Maybe String
mainMaybe = do
  name <- Map.lookup "name" userInput
  let statement = helloPerson name
  return statement
