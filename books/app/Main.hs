{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad (void)
import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.GI.Base
import Data.Int (Int32)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), close, execute, field, open, query)
import qualified GI.Gtk as Gtk
import Network.HTTP.Client (Manager, Request (host, method, path, port, queryString, secure), Response (responseBody, responseStatus), defaultRequest, httpLbs, managerSetProxy, newManager, parseRequest, proxyEnvironment)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Book = Book
  { id :: Maybe Int,
    bibKey :: String,
    infoUrl :: String,
    preview :: String,
    previewUrl :: String,
    thumbnailUrl :: String,
    createdAt :: Maybe UTCTime
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

downloadThumb :: String -> String -> IO ()
downloadThumb url isbn = do
  man <- withHttpConnection
  request <- parseRequest url
  response <- httpLbs request man
  let body = responseBody response
      status = responseStatus response
  putStrLn $ "Response status code: " <> show status
  BL.writeFile (isbn <> ".jpg") body
  putStrLn "Download finished"

getBookFromDB :: Connection -> String -> IO (Maybe Book)
getBookFromDB conn isbn = do
  resp <-
    query conn "SELECT * FROM book WHERE bib_key = (?)" (Only $ "ISBN:" <> isbn) :: IO [Book]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : _) = Just x

withHttpConnection :: IO Manager
withHttpConnection = do
  let settings =
        managerSetProxy
          (proxyEnvironment Nothing)
          tlsManagerSettings
  newManager settings

getBookFromNet :: BC.ByteString -> IO (Response BL.ByteString)
getBookFromNet isbn = do
  man <- withHttpConnection
  let initialRequest =
        defaultRequest
  let req =
        initialRequest
          { method = "GET",
            port = 443,
            host = libHost,
            secure = True,
            path = booksPath,
            queryString = "bibkeys=ISBN%3A" <> isbn <> "&format=json"
          }
  httpLbs req man

getInnerBook :: String -> Map.Map String Book -> Maybe Book
getInnerBook isbn = Map.lookup ("ISBN:" <> isbn)

getBook :: String -> IO (Maybe Book)
getBook isbn = withDBConnSelect $ \conn -> do
  resp <- getBookFromDB conn isbn
  case resp of
    Just _ -> do return resp
    Nothing -> do
      bookFromNet <- getBookFromNet (BC.pack isbn)
      let body = responseBody bookFromNet
      let book = decode body >>= getInnerBook isbn
      case book of
        Just b -> do
          saveBook b
          downloadThumb (thumbnailUrl b) isbn
          getBookFromDB conn isbn
        Nothing -> return Nothing

toInt32 :: Int -> Int32
toInt32 = fromIntegral

activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  label <- new Gtk.Label [#label := "ISBN"]
  info <- new Gtk.TextView []
  #append box label
  textField <- new Gtk.Entry []
  #append box textField
  button <-
    new
      Gtk.Button
      [#label := "Search"]
  _ <- on button #clicked $ do
    s <- Gtk.entryGetBuffer textField
    txt <- Gtk.entryBufferGetText s
    book <- getBook (T.unpack txt)
    textBuff <- new Gtk.TextBuffer []
    case book of
      Just b -> #setText textBuff prUrl (toInt32 (T.length prUrl))
        where
          prUrl = T.pack (show b)
      Nothing -> #setText textBuff text (toInt32 (T.length text))
        where
          text = T.pack "Nothing found"
    #setBuffer info (Just textBuff)
  #append box button
  #append box info
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "Books",
        #child := box
      ]
  window.show -- or window.showAll if using gi-gtk3 instead of gi-gtk4

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [ #applicationId := "books",
        On #activate (activate ?self)
      ]

  void $ app.run Nothing
