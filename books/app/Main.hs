{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), close, execute, field, open, query)
import qualified GI.Gtk as Gtk
import Network.HTTP.Client (Request (host, method, path, port, queryString, secure), Response (responseBody), defaultRequest, httpLbs, managerSetProxy, newManager, proxyEnvironment)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Book = Book
  { id :: Maybe Int
  , bibKey :: String
  , infoUrl :: String
  , preview :: String
  , previewUrl :: String
  , thumbnailUrl :: String
  , createdAt :: Maybe UTCTime
  }
  deriving (Show)

instance FromJSON Book where
  parseJSON (Object v) =
    Book
      <$> v .:! "NA"
      <*> v .: "bib_key"
      <*> v .: "info_url"
      <*> v .: "preview"
      <*> v .: "preview_url"
      <*> v .: "thumbnail_url"
      <*> v .:! "Nothing"
  parseJSON _ = empty

libHost :: BC.ByteString
libHost = "openlibrary.org"

booksPath :: BC.ByteString
booksPath = "/api/books"

withDBConn :: (Connection -> IO ()) -> IO ()
withDBConn action = do
  conn <- open "books.db"
  action conn
  close conn

withDBConnSelect :: (Connection -> IO (Maybe a)) -> IO (Maybe a)
withDBConnSelect action = do
  conn <- open "books.db"
  res <- action conn
  close conn
  return res

instance FromRow Book where
  fromRow =
    Book
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

saveBook :: Book -> IO ()
saveBook book = withDBConn $ \conn -> do
  createdTime <- getCurrentTime
  execute
    conn
    "INSERT INTO book (bib_key, info_url, preview, preview_url, thumbnail_url, created_at) VALUES(?,?,?,?,?,?);"
    (bibKey book, infoUrl book, preview book, previewUrl book, thumbnailUrl book, createdTime)

getBookFromDB :: Connection -> String -> IO (Maybe Book)
getBookFromDB conn isbn = do
  resp <-
    query conn "SELECT * FROM book WHERE bib_key = (?)" (Only $ "ISBN:" <> isbn) :: IO [Book]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

getBookFromNet :: BC.ByteString -> IO (Response BL.ByteString)
getBookFromNet isbn = do
  let settings =
        managerSetProxy
          (proxyEnvironment Nothing)
          tlsManagerSettings
  let initialRequest =
        defaultRequest
  man <- newManager settings
  let req =
        initialRequest
          { method = "GET"
          , port = 443
          , host = libHost
          , secure = True
          , path = booksPath
          , queryString = "bibkeys=ISBN%3A" <> isbn <> "&format=json"
          }
  httpLbs req man

getInnerBook :: String -> Map.Map String Book -> Maybe Book
getInnerBook isbn = Map.lookup ("ISBN:" <> isbn)

getBook :: BC.ByteString -> IO (Maybe Book)
getBook isbn = withDBConnSelect $ \conn -> do
  resp <- getBookFromDB conn (BC.unpack isbn)
  case resp of
    Just _ -> do return resp
    Nothing -> do
      bookFromNet <- getBookFromNet isbn
      let body = responseBody bookFromNet
      let book = decode body >>= getInnerBook (BC.unpack isbn)
      case book of
        Just b -> do
          saveBook b
          getBookFromDB conn (BC.unpack isbn)
        Nothing -> return Nothing
activate :: Gtk.Application -> IO ()
activate app = do 
  window <- new Gtk.ApplicationWindow [#application := app,
                                       #title = "Hi there"]
  window.show                                       
main :: IO ()
main = do
  putStrLn "Enter ISBN"
  isbn <- BC.getLine
  book <- getBook isbn
  case book of
    Just b -> do
      print b
    Nothing -> print ("Book not found" :: String)
