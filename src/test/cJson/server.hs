-- file: server.hs
{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Data.Aeson
import Data.Map
import System.IO
import Data.Maybe
import Control.Concurrent

import Data.ByteString as ByteString

type S_handler = String -> IO ()

serverRun :: S_handler -> IO ()
serverRun sHandler = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing
               (Just "8011")
  let serverAddr = Prelude.head addrInfos

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)

  listen sock 5

  procRequests sock

  where
    -- | Process incoming connection requests
    procRequests :: Socket -> IO ()
    procRequests sock = do
      (connsock, clientaddr) <- accept sock

      setSocketOption connsock NoDelay 1

      forkIO $ procMsgs connsock
      procRequests sock

    procMsgs :: Socket -> IO ()
    procMsgs sock = do
      h <- socketToHandle sock ReadWriteMode
      hSetBinaryMode h True
      procLoopWorks h

    procLoopWorks :: Handle -> IO ()
    procLoopWorks h = do
      isOpen <- hIsOpen h
      case isOpen of
        True -> do
          hSetBuffering h NoBuffering
          msg <- ByteString.hGetLine h

          let json =  decodeStrict msg :: Maybe (Map String String)
          if isNothing json
            then procLoopWorks h
            else (print $ (fromJust json)) >> procLoopWorks h
        False -> return ()

main = serverRun (\x -> print x)
