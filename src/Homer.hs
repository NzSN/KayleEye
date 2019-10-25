-- file: Homer.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Homer where

import KayleConst
import Room
import Letter

-- Json
import Data.Aeson
import Data.Map as Map
import Data.Maybe
import Data.List as List

import GHC.Generics

-- Testing purpose
import Control.Concurrent
import Test.HUnit
import Control.Concurrent.Thread.Delay
import qualified Control.Exception as Ex

-- Socket
import Network.Socket
import System.IO
import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Lazy.Internal as BI
import Data.Text as Text
import Network.Socket.ByteString as BS

import Data.String.Conversions (cs)


-- Homer
data Homer = Homer { handle :: Handle } | Empty_Homer

pickHomer :: String -- HostName
          -> String -- Port
          -> IO Homer
pickHomer host port = do
    addrInfos <- getAddrInfo Nothing (Just host) (Just port)

    let serverAddr = Prelude.head addrInfos
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol

    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serverAddr)

    handle <- socketToHandle sock ReadWriteMode
    return $ Homer handle

releaseHomer :: Homer -> IO ()
releaseHomer h = hClose $ handle h

sendLetter :: Homer -> Letter -> IO Integer
sendLetter h l = do
  let handle_ = handle h
      letterStr = cs $ letterBuild l
  hPutStrLn handle_ letterStr
  hFlush handle_
  return k_ok

waitLetter :: Homer -> IO Letter
waitLetter h = do
  let handle_ = handle h

  letterStr <- hGetLine handle_
  let letterMaybe = decode (cs letterStr)
  if isNothing letterMaybe
    then return Empty_letter
    else return $ fromJust letterMaybe

waitHomer :: Socket -> IO Homer
waitHomer socket = do
  (connsock, _) <- accept socket

  setSocketOption connsock KeepAlive 1
  setSocketOption connsock tcp_keepidle 10
  setSocketOption connsock tcp_keepintvl 1
  setSocketOption connsock tcp_keepcnt 3

  handle <- socketToHandle connsock ReadWriteMode
  hSetBuffering handle LineBuffering

  return $ Homer handle

  where tcp_keepidle = CustomSockOpt (6, 4)
        tcp_keepintvl = CustomSockOpt (6, 5)
        tcp_keepcnt = CustomSockOpt (6, 6)

withGuard :: Ex.Exception e => (e -> IO Letter) -> IO Letter -> IO Letter
withGuard handler io_l = Ex.handle (\e -> print e >> handler e) io_l

-- Test cases
homerTest :: Test
homerTest = TestList [TestLabel "Send and recv" (TestCase homerAssert),
                      TestLabel "Letter update" (TestCase homerAssert1),
                      TestLabel "Letter updates" (TestCase homerAssert2),
                      TestLabel "Letter Header Retri" (TestCase homerAssert3)]
  where homerAssert = do
          -- Create sock
          (addr:xs) <- getAddrInfo Nothing (Just "127.0.0.1") (Just "8013")
          sock <- socket (addrFamily addr) Stream defaultProtocol
          bind sock (addrAddress addr)
          listen sock 10

          m1 <- newEmptyMVar
          m2 <- newEmptyMVar

          -- Server
          forkIO $ do
            homer <- waitHomer sock
            l <- waitLetter homer

            sendLetter homer l

            putMVar m1 ()

          -- Client
          forkIO $ do
            homer <- pickHomer "127.0.0.1" "8013"
            sendLetter homer l

            rl <- waitLetter homer

            putMVar m2 ()

          takeMVar m1
          takeMVar m2

        l = Letter (ident2Str $ Identity "item1" "12345" "merge")
            (fromList [("iid", "1")])
            (fromList [("T1", "T")])
        l1 = Letter (ident2Str $ Identity "item1" "12345" "merge")
             (fromList [("iid", "1")])
             (fromList [("T1", "T"), ("T2", "T")])

        check l_ m =
          let identCheck = (ident l_) == (ident l)
              headerCheck = (Map.lookup "iid" (header l_)) == (Map.lookup "iid" (header l))
              contentCheck = (Map.lookup "T1" (content l_)) == (Map.lookup "T1" (content l))
          in if identCheck && headerCheck && contentCheck
             then putMVar m 't'
             else putMVar m 'f'

        homerAssert1 = do
          let updated_value = "UPDATED_VALUE"
              l_u = letterUpdate l "T1" updated_value
          assertEqual "Letter update" (Just updated_value) (retriFromContent l_u "T1")

        homerAssert2 = do
          let updated_value = "UPDATED_VALUE"
              l_u = letterUpdate' l1 [("T1", updated_value), ("T2", updated_value)]
          assertEqual "Letter updates" True ((retriFromContent l_u "T1") == (Just updated_value) &&
                                             (retriFromContent l_u "T2") == (Just updated_value))

        homerAssert3 = do
          assertEqual "Letter Header Retri" (Just "1") (retriFromHeader l "iid")
