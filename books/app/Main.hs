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
import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow (fromRow), Only (Only), close, execute, field, open, query)
import GI.Gtk
import qualified GI.Gtk as Gtk
import Network.HTTP.Client (Manager, Request (host, method, path, port, queryString, secure), Response (responseBody, responseStatus), defaultRequest, httpLbs, managerSetProxy, newManager, parseRequest, proxyEnvironment)
import Network.HTTP.Client.TLS (tlsManagerSettings)

data Book = Book
  { id :: Maybe Int,
    bibKey :: String,
    infoUrl :: String,
    preview :: String,
    previewUrl :: String,
    thumbnailUrl :: Maybe String,
    createdAt :: Maybe UTCTime
  }
  deriving (Show)

displayBook :: Book -> T.Text
displayBook b =
  "bib key: "
    <> T.pack (bibKey b)
    <> "\n"
    <> "info url: "
    <> T.pack (infoUrl b)

instance FromJSON Book where
  parseJSON (Object v) =
    Book
      <$> v .:! "NA"
      <*> v .: "bib_key"
      <*> v .: "info_url"
      <*> v .: "preview"
      <*> v .: "preview_url"
      <*> v .:! "thumbnail_url"
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
downloadThumb isbn url = do
  putStrLn $ "Downloading image from " <> url
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
      let status = responseStatus bookFromNet
      print $ "Response status for get book is " <> show status
      print body
      let book = decode body >>= getInnerBook isbn

      case book of
        Just b -> do
          saveBook b
          let thumb = thumbnailUrl b
          if isNothing thumb
            then print ("No thumb for image" :: String)
            else downloadThumb isbn (largeImageUrl (fromJust thumb))
          getBookFromDB conn isbn
        Nothing -> return Nothing

toInt32 :: Int -> Int32
toInt32 = fromIntegral

largeImageUrl :: String -> String
largeImageUrl thumbUrl =
  if isSmallImage
    then take (length thumbUrl - 6) thumbUrl <> "-L.jpg"
    else thumbUrl
  where
    isSmallImage = "-S.jpg" `isSuffixOf` thumbUrl

activate :: Gtk.Application -> IO ()
activate app = do
  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
  infoBox <- new Gtk.Grid [#orientation := Gtk.OrientationHorizontal]
  label <- new Gtk.Label [#label := "ISBN"]
  info <- new Gtk.TextView []
  pic <- new Gtk.Picture [#canShrink := True, #hexpand := True, #vexpand := True]
  #append box label
  textField <- new Gtk.Entry []
  #append box textField
  button <-
    new
      Gtk.Button
      [#label := "Search"]
  _ <- on button #clicked $ do
    s <- Gtk.entryGetBuffer textField
    isbn <- Gtk.entryBufferGetText s
    print $ "ISBN: " <> isbn
    book <- getBook (T.unpack isbn)
    print book
    textBuff <- new Gtk.TextBuffer []
    case book of
      Just b -> do
        #setText textBuff bookInfoTxt (toInt32 (T.length bookInfoTxt))
        let thumb = thumbnailUrl b
        if isJust thumb
          then #setFilename pic (Just (T.unpack isbn <> ".jpg"))
          else print ("No thumb image" :: String)
        where
          bookInfoTxt = displayBook b
      Nothing -> #setText textBuff text (toInt32 (T.length text))
        where
          text = T.pack "Nothing found"
    #setBuffer info (Just textBuff)
  scrollWin <- new Gtk.ScrolledWindow []
  #setChild scrollWin $ Just info
  #setHexpandSet scrollWin True
  #setHexpand scrollWin True
  #insertRow infoBox 0
  #insertRow infoBox 1
  #insertColumn infoBox 0
  #insertColumn infoBox 1
  #attach infoBox pic 0 0 1 1
  #attachNextTo infoBox scrollWin (Just pic) PositionTypeRight 1 1
  #append box button
  #append box infoBox
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := "Books",
        #child := box
      ]
  window.show

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [ #applicationId := "books",
        On #activate (activate ?self)
      ]

  void $ app.run Nothing
