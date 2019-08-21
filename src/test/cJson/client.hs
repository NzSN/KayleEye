-- file: client.hs

{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Data.Aeson
import System.IO
import Data.ByteString.Lazy.Char8 as ByteString


connectToServer :: IO Handle
connectToServer = do
  addrInfos <- getAddrInfo Nothing (Just "localhost") (Just "8011")
  let serverAddr = Prelude.head addrInfos

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol

  setSocketOption sock KeepAlive 1
  setSocketOption sock NoDelay 1

  connect sock (addrAddress serverAddr)

  h <- socketToHandle sock WriteMode
  hSetBuffering h (BlockBuffering Nothing)
  return h

sendMsg :: Handle -> String -> IO ()
sendMsg h m =
  let json = encode $ object [ "name" .= m ]
  in ByteString.hPutStrLn h json >> hFlush h

disconnectFromServer :: Handle -> IO ()
disconnectFromServer h = hClose h
