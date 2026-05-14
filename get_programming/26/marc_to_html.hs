{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding as E
import Data.Text.Encoding qualified as B
import Data.Text.IO qualified as TIO

type Author = T.Text

type Title = T.Text

data Book = Book
  { author :: Author,
    title :: Title
  }
  deriving (Show)

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book =
  mconcat
    [ "<p>\n",
      titlePart,
      authorPart,
      "</p>\n"
    ]
  where
    titlePart = mconcat ["<strong>", title book, "</strong>\n"]
    authorPart = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books =
  mconcat
    [ "<html>\n",
      "<head>\n",
      "<title>Books</title>\n",
      "<meta charset='utf-8'/>\n",
      "</head>\n",
      "<body>\n",
      booksHtml,
      "</body>\n",
      "</html>"
    ]
  where
    booksHtml = mconcat (map bookToHtml books)

book1 :: Book
book1 =
  Book
    { author = "Author1",
      title = "Title1"
    }

book2 :: Book
book2 =
  Book
    { author = "Author2",
      title = "Title2"
    }

book3 :: Book
book3 =
  Book
    { author = "Author3",
      title = "Title3"
    }

books :: [Book]
books = [book1, book2, book3]

type MarcRecordRaw = B.ByteString

type MarcLeaderRaw = B.ByteString

leaderLength :: Int
leaderLength = 25

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest bytes = (record, rest)
  where
    recordLength = getRecordLength (getLeader bytes)
    (record, rest) = B.splitAt recordLength bytes

allRecords :: B.ByteString -> [B.ByteString]
allRecords bytes =
  if bytes == B.empty
    then []
    else next : allRecords rest
  where
    (next, rest) = nextAndRest bytes

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt baseAddrRaw
  where
    baseAddrRaw = B.take 5 remainder
    remainder = B.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = baseAddr - (leaderLength + 1)
  where
    baseAddr = getBaseAddress leader

type MarcDirectoryRaw = B.ByteString

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader
  where
    directoryLength = getDirectoryLength leader
    leader = getLeader record
    afterLeader = B.drop leaderLength record

type MarcDirectoryEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if rest == B.empty
    then []
    else entry : splitDirectory rest
  where
    (entry, rest) = B.splitAt dirEntryLength directory

data FieldMetadata = FieldMetadata
  { tag :: T.Text,
    fieldLength :: Int,
    fieldStart :: Int
  }
  deriving (Show)

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry =
  FieldMetadata
    { tag = textTag,
      fieldLength = theLength,
      fieldStart = theStart
    }
  where
    (tagRaw, rest) = B.splitAt 3 entry
    textTag = B.decodeUtf8 tagRaw
    (rawLength, rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

main :: IO ()
main = do
  content <- B.readFile "records.mrc"
  let records = allRecords content
  print (length records)
  let aRecord = head records
  let directory = getDirectory aRecord
  let dirEntries = splitDirectory directory
  print (length dirEntries)
