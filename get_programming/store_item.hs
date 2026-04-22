type FirstName = String
type LastName = String
type MiddleName = String
data Name = Name FirstName LastName
   | NameWithMiddleName FirstName MiddleName LastName
   | TwoInitialsWithLast Char Char LastName
   | FirstNameWithTwoInits FirstName Char Char
data Author = Author Name
data Artist = Person Name | Band String
data Contact = Contact String
data Creator = AuthorCreator Author | ArtistCreator Artist | ContactCreator Contact
data Book = Book {
  author :: Creator
, isbn :: String
, bookTitle :: String
, bookYear :: Int
, bookPrice :: Double
}
data VinylRecord = VinylRecord {
    artist :: Creator
  , recordTitle :: String
  , recordYear :: Int
  , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
     name :: String
   , toyDescription :: String
   , toyPrice :: Double
}

data Pamphlet = Pamphlet {
     pamphletTitle :: String
  ,  pamphletDescription :: String
  ,  pamphletContact :: Creator
}
data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet
price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price _ = 0

