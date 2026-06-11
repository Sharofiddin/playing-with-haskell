module Main (main) where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import Data.Maybe
import Data.Text as T
import GHC.Generics (Generic)

data Book = Book
  { author :: T.Text,
    title :: T.Text,
    year :: Int
  }
  deriving (Show, Generic)

instance FromJSON Book

instance ToJSON Book

myBook :: Book
myBook = Book {title = "Learn Haskell", author = "Will Kurt", year = 2017}

myBookJson :: BC.ByteString
myBookJson = encode myBook

rawJson :: BC.ByteString
rawJson = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJson :: Maybe Book
bookFromJson = decode rawJson

data NOAAResult = NOAAResult
  { uid :: T.Text,
    mindate :: T.Text,
    maxdate :: T.Text,
    name :: T.Text,
    datacoverage :: Float,
    resultId :: T.Text
  }
  deriving (Show)

instance FromJSON NOAAResult where
  parseJSON (Object v) =
    NOAAResult
      <$> v .: "uid"
      <*> v .: "mindate"
      <*> v .: "maxdate"
      <*> v .: "name"
      <*> v .: "datacoverage"
      <*> v .: "id"

instance ToJSON NOAAResult where
  toJSON record =
    object
      [ "uid" .= uid record,
        "mindate" .= mindate record,
        "maxdate" .= maxdate record,
        "name" .= name record,
        "datacoverage"
          .= datacoverage record,
        "id"
          .= resultId record
      ]

data ResultSet = ResultSet
  { offset :: Int,
    count :: Int,
    limit :: Int
  }
  deriving (Show, Generic)

instance FromJSON ResultSet

instance ToJSON ResultSet

data Metadata = Metadata
  { resultset :: ResultSet
  }
  deriving (Show, Generic)

instance FromJSON Metadata

instance ToJSON Metadata

data NOAAResponse = NOAAResponse
  { metadata :: Metadata,
    results :: [NOAAResult]
  }
  deriving (Show, Generic)

instance FromJSON NOAAResponse

instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just nooaResults) = do
  forM_ nooaResults (print . name)

data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)

instance FromJSON IntList

instance ToJSON IntList

intListExample :: IntList
intListExample =
  Cons 1 $
    Cons 2 EmptyList

main :: IO ()
main = do
  jsonData <- B.readFile "data.json"
  let noaaResponse = decode jsonData :: Maybe NOAAResponse
  let noaaResults = results <$> noaaResponse
  printResults noaaResults
  print (encode noaaResponse)
