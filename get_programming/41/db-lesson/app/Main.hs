module Main (main) where

import Data.Bits (Bits (xor))
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.SQLite.Simple (Connection, FromRow, Only (Only), Query, close, execute, field, open, query, query_)
import Database.SQLite.Simple.FromRow
import Lib

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesBorrowed :: Int
  }

data User = User
  { userId :: Int,
    userName :: String
  }

instance Show User where
  show user =
    mconcat
      [ show $ userId user,
        ".) ",
        userName user
      ]

instance Show Tool where
  show tool =
    mconcat
      [ show $ toolId tool,
        ").",
        name tool,
        "\n description: ",
        description tool,
        "\n last returned: ",
        show $ lastReturned tool,
        "\n times borrowed: ",
        show $ timesBorrowed tool
      ]

withConn :: String -> (Connection -> IO ()) -> IO ()
withConn dbName action = do
  conn <- open dbName
  action conn
  close conn

addUserAction :: String -> Connection -> IO ()
addUserAction username conn = do
  execute conn "INSERT INTO users (username) VALUES (?)" (Only username)
  print "user added"

addUser :: String -> IO ()
addUser username =
  withConn "tools.db" (addUserAction username)

addTool :: String -> String -> IO ()
addTool toolName toolDescription = withConn "tools.db" $
  \conn -> do
    currentDay <- utctDay <$> getCurrentTime
    execute
      conn
      "INSERT INTO tools (name, description , lastReturned, timesBorrowed) VALUES (?,?,?,?);"
      (toolName, toolDescription, currentDay, 0 :: Int)

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn "tools.db" $
  \conn -> do
    execute
      conn
      "INSERT INTO checkedout (user_id, tool_id) VALUES (?,?)"
      (userId, toolId)

instance FromRow User where
  fromRow =
    User
      <$> field
      <*> field

instance FromRow Tool where
  fromRow =
    Tool
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field

printUsers :: IO ()
printUsers = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn "SELECT * FROM users;" :: IO [User]
    mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q = withConn "tools.db" $
  \conn -> do
    resp <- query_ conn q :: IO [Tool]
    mapM_ print resp

printTools :: IO ()
printTools = printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable =
  printToolQuery $
    mconcat
      [ "SELECT * FROM tools ",
        "WHERE id NOT IN ",
        "(SELECT tool_id FROM checkedout);"
      ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
  resp <-
    query
      conn
      "SELECT * FROM tools WHERE id = (?)"
      (Only toolId) ::
      IO [Tool]
  return $ firstOrNothing resp

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x : xs) = Just x

printCheckedOut :: IO ()
printCheckedOut =
  printToolQuery $
    mconcat
      [ "SELECT * FROM tools ",
        "WHERE id IN ",
        "(SELECT tool_id FROM checkedout);"
      ]

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  tool
    { lastReturned = date,
      timesBorrowed = timesBorrowed tool + 1
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = print "id not found"
updateOrWarn (Just tool) = withConn "tools.db" $
  \conn -> do
    let q =
          mconcat
            [ "UPDATE tools SET ",
              "lastReturned  = ?, ",
              "timesBorrowed = ? ",
              "WHERE id = ?;"
            ]
    execute
      conn
      q
      ( lastReturned tool,
        timesBorrowed tool,
        toolId tool
      )
    print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn "tools.db" $
  \conn -> do
    tool <- selectTool conn toolId
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool = updateTool <$> tool <*> pure currentDay
    updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolId = withConn "tools.db" $
  \conn -> do
    execute
      conn
      "DELETE FROM checkedout WHERE tool_id = (?);"
      (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = do
  checkin toolId
  updateToolTable toolId

promptAndAddUser :: IO ()
promptAndAddUser = do
  print "Enter new username: "
  username <- getLine
  addUser username

promptAndCheckout :: IO ()
promptAndCheckout = do
  print "Enter the id of the user:"
  checkoutUserId <- read <$> getLine
  print "Enter tool id:"
  checkoutToolId <- read <$> getLine
  checkout checkoutUserId checkoutToolId

promptAndCheckin :: IO ()
promptAndCheckin = do
  print "Enter tool id to checkin:"
  checkinToolId <- read <$> getLine
  checkinAndUpdate checkinToolId

promptAndAddTool :: IO ()
promptAndAddTool = do
  print "Tool name:"
  toolName <- getLine
  print "Tool description:"
  description <- getLine
  addTool toolName description

performCommand :: String -> IO ()
performCommand command
  | command == "users" = printUsers >> main
  | command == "tools" = printTools >> main
  | command == "addUser" = promptAndAddUser >> main
  | command == "addTool" = promptAndAddTool >> main
  | command == "checkout" = promptAndCheckout >> main
  | command == "checkin" = promptAndCheckin >> main
  | command == "in" = printAvailable >> main
  | command == "out" = printCheckedOut >> main
  | command == "quit" = print "bye!"
  | otherwise = print "Sorry command not found" >> main

main :: IO ()
main = do
  print "Enter a command"
  command <- getLine
  performCommand command
