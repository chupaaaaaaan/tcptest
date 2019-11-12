{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import qualified Data.ByteString as B
import Network.Socket.ByteString
import System.Environment
import System.IO

import My.Tcp.Client

main :: IO ()
main = do
  args <- liftIO getArgs
  if length args /= 2
    then hPutStrLn stderr "Invalid number of args: 2"
    else do let [host,port] = args
            runTCPClient host port $ \s -> (`runContT` return) $ do
              hs <- ContT $ withFile "client_send.txt" ReadMode
              hr <- ContT $ withFile "client_recv.txt" WriteMode

              liftIO $ sendMsg s hs
              liftIO $ recvMsg s hr

  where sendMsg s h = do
          msg <- B.hGetContents h
          sendAll s msg
          -- sendAll s $ B.singleton 0

        recvMsg s h = do
          msg <- recv s 1024
          if B.last msg == 0
            then B.hPut h $ B.init msg
            else do B.hPut h msg
                    recvMsg s h
