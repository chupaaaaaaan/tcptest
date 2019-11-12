module My.Tcp.Client where

import qualified Control.Exception as E
import Network.Socket

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = do
  addr <- resolve
  E.bracket (open addr) close client

  where resolve = do
          let hints = defaultHints { addrSocketType = Stream }
          head <$> getAddrInfo (Just hints) (Just host) (Just port)
    
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          return sock
