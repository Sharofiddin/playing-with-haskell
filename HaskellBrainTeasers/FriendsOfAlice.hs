module HaskellBrainTeasers.FriendsOfAlice where

import Data.List (intercalate)

data Person = Person {name :: String, friends :: [Person]} deriving (Show)

alice :: Person
alice = Person "Alice" [bob]

bob :: Person
bob = Person "Bob" [alice]

main :: IO ()
main = do
  let aliceFriends = name <$> friends alice
      bobFriends = name <$> friends bob
  putStrLn $ "Friends of alice " <> intercalate "," aliceFriends
  putStrLn $ "Friends of bob " <> intercalate "," bobFriends
