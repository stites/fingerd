{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeperated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeperated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
   "Name: " , e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t",
   "Shell: "    , e shell  , "\n"]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " ++ (show username))
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got a connection, handling query"
  handleQuery dbConn soc
  sClose soc

withFingerSocket :: (Socket -> IO ()) -> IO ()
withFingerSocket action = withSocketsDo $ do
  let addrInfoDefault = Just (defaultHints {addrFlags = [AI_PASSIVE]})
      serviceName     = Just "79"
      hostName        = Nothing
  addrinfos <- getAddrInfo addrInfoDefault hostName serviceName
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  action sock
  sClose sock

main :: IO ()
main = do
  createDatabase
  withFingerSocket $ \sock -> do
    conn <- open "finger.db"
    handleQueries conn sock
    SQLite.close conn

data User = User {
    userId        :: Integer
  , username      :: Text
  , shell         :: Text
  , homeDirectory :: Text
  , realName      :: Text
  , phone         :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
instance ToRow User where
  toRow (User id_  username  shell  homeDir  realName  phone) =
       toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  ( id            INTEGER PRIMARY KEY AUTOINCREMENT
  , username      TEXT    UNIQUE
  , shell         TEXT
  , homeDirectory TEXT
  , realName      TEXT
  , phone         TEXT
)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?,?,?,?,?,?)"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return (Just user)
    _ -> throwIO DuplicateData

defaultRow :: UserRow
defaultRow = (Null, "stitess", "/bin/bash", "/Users/stitess", "Sam Stites", "123.456.7890")

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute  conn insertUser defaultRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

