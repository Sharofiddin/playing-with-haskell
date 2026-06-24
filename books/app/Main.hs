{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encoding (value)
import Data.Aeson.KeyMap (mapKeyVal)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)
import GHC.Base (KindRep (KindRepVar))
import Network.HTTP.Client (Request (host, method, path, port, queryString, secure), RequestBody (RequestBodyBuilder), Response (responseBody), defaultRequest, httpLbs, managerSetProxy, newManager, parseRequest, proxyEnvironment)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Book = Book
  { bibKey :: String
  , infoUrl :: String
  , preview :: String
  , previewUrl :: String
  , thumbnailUrl :: String
  }
  deriving (Show)

instance FromJSON Book where
  parseJSON (Object v) =
    Book
      <$> v .: "bib_key"
      <*> v .: "info_url"
      <*> v .: "preview"
      <*> v .: "preview_url"
      <*> v .: "thumbnail_url"
  parseJSON _ = empty
libHost :: BC.ByteString
libHost = "openlibrary.org"

booksPath :: BC.ByteString
booksPath = "/api/books"

getInnerBook :: Map.Map String Book -> String -> Maybe Book
getInnerBook bookWrapper isbn =
  Map.lookup ("ISBN:" <> isbn) bookWrapper

booksURL :: BC.ByteString
booksURL = libHost <> booksPath
main :: IO ()
main = do
  let settings =
        managerSetProxy
          (proxyEnvironment Nothing)
          tlsManagerSettings
  putStrLn "Enter ISBN"
  isbn <- BC.getLine
  man <- newManager settings
  let initialRequest =
        defaultRequest
  let req =
        initialRequest
          { method = "GET"
          , port = 443
          , host = libHost
          , secure = True
          , path = booksPath
          , queryString = "bibkeys=ISBN%3A" <> isbn <> "&format=json"
          }
  res <- httpLbs req man
  let body = responseBody res
  let jsonBody =
        (<$>)
          getInnerBook
          (decode body)
          <*> Just (BC.unpack isbn)
  print jsonBody
