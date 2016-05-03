module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do     -- ^ loop this forever with the y-combinator
  (soc, _) <- accept sock          -- ^ block until a connection to the server is made
  printAndKickback soc             -- ^ do stuff
  sClose soc                       -- ^ close our connection
  where printAndKickback conn = do
          msg <- recv conn 1024    -- ^ get up to 1024 bytes of text from the connection
          print msg                -- ^ print it literally
          sendAll conn msg         -- ^ relay the message back to the connection

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
main = withFingerSocket logAndEcho

