{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import qualified Data.ByteString as B
import Network.Socket.ByteString (recv,sendAll)
import System.IO

import My.Tcp.Server

main :: IO ()
main = runTCPServer Nothing "8001" echo

  where echo s = (`runContT` return) $ do
          hr <- ContT $ withFile "server_recv.txt" ReadWriteMode
          hs <- ContT $ withFile "server_send.txt" ReadWriteMode
          liftIO $ recvMsg s hr

          liftIO $ hSeek hr AbsoluteSeek 0
          liftIO (B.hGetContents hr) >>= liftIO . B.hPut hs
          liftIO $ hSeek hs AbsoluteSeek 0

          liftIO $ sendMsg s hs

        recvMsg s h = do
          msg <- recv s 1024
          if B.last msg == 0
            then B.hPut h $ B.init msg
            else do B.hPut h msg
                    recvMsg s h
            
        sendMsg s h = do
          msg <- B.hGetContents h
          sendAll s msg
          sendAll s $ B.singleton 0
