{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B
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
leaderLength = 24

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

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record metadata =
  E.decodeUtf8 fieldRaw
  where
    baseAddress = getBaseAddress record
    baseRaw = B.drop baseAddress record
    fieldRaw = B.take (fieldLength metadata) (B.drop (fieldStart metadata) baseRaw)

fieldDelimter :: Char
fieldDelimter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if null results
    then Nothing
    else Just (head results)
  where
    metadata = (getFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record =
  if null results
    then Nothing
    else Just ((T.drop 1 . head) results)
  where
    rawField = getTextField record fieldMetadata
    subfields = T.split (== fieldDelimter) rawField
    results = filter ((== subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record =
  lookupSubfield entryMetadata subfield record
  where
    entryMetadata = lookupFieldMetadata aTag record

lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor = lookupValue authorTag authorSubfield

lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle = lookupValue titleTag titleSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
  where
    records = allRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
  map
    ( \(title, author) ->
        Book
          { title = fromJust title,
            author = fromJust author
          }
    )
    justPairs
  where
    justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

recordToBook :: MarcRecordRaw -> Maybe Book
recordToBook record =
  if isNothing maybeAuthor && isNothing maybeTitle
    then Nothing
    else Just (Book (fromMaybe "" maybeAuthor) (fromMaybe "" maybeTitle))
  where
    maybeAuthor = lookupAuthor record
    maybeTitle = lookupTitle record

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
  content <- B.readFile "records.mrc"
  let processed = processRecords 500 content
  TIO.writeFile "books.html" processed
