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

main :: IO ()
main = putStrLn "hello world!"

