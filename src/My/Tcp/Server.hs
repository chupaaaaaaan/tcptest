module My.Tcp.Server where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Network.Socket

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = do
  addr <- resolve
  E.bracket (open addr) close loop

  where resolve = do
          let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
          head <$> getAddrInfo (Just hints) mhost (Just port)

        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          setSocketOption sock ReuseAddr 1
          bind sock $ addrAddress addr
          setCloseOnExecIfNeeded $ fdSocket sock
          listen sock 10
          return sock

        loop sock = forever $ do
          putStrLn "Waiting connection from client..."
          (conn, peer) <- accept sock
          putStrLn "Connected accepted."
          putStrLn $ "Connection from " <> show peer
          void $ forkFinally (server conn) (const $ close' conn)
          putStrLn "Connection closed."
