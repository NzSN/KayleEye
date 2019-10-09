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

-- Socket
import Network.Socket
import System.IO
import Data.ByteString.Lazy hiding (putStrLn)
import Data.ByteString.Lazy.Internal as BI
import Data.Text as Text
import Network.Socket.ByteString as BS

import Data.String.Conversions (cs)


-- Homer
data Homer = Homer { handle :: Handle, room :: Room, letter :: Letter } | Empty_Homer

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
    return $ Homer handle Empty_Room Empty_letter

letterBuild :: Letter -> ByteString
letterBuild l = encode $ Letter (ident l) (header l) (content l)

sendLetter :: Homer -> Letter -> IO Integer
sendLetter h l = do
  let handle_ = handle h
      letterStr = cs $ letterBuild l
  hPutStrLn handle_ letterStr
  return k_ok

waitLetter :: Homer -> IO Letter
waitLetter h = do
  let handle_ = handle h

  letterStr <- hGetLine handle_
  let letterMaybe = decode (cs letterStr)
  if isNothing letterMaybe
    then return Empty_letter
    else return $ fromJust letterMaybe

waitHomer :: Socket -> Room -> IO Homer
waitHomer socket room = do
  (connsock, _) <- accept socket
  handle <- socketToHandle connsock ReadWriteMode
  hSetBuffering handle LineBuffering

  return $ prepareH $ Homer handle room


-- Protocol
prepareH :: Homer -> Homer
prepareH h = h

-- Test cases
homerTest :: Test
homerTest = TestList [TestLabel "Send and recv" (TestCase homerAssert),
                      TestLabel "Letter update" (TestCase homerAssert1),
                      TestLabel "Letter updates" (TestCase homerAssert2),
                      TestLabel "Letter Header Retri" (TestCase homerAssert3)]
  where homerAssert = do
          homer_recv <- pickHomer "localhost" "8011"
          homer_send <- pickHomer "localhost" "8011"

          m <- newEmptyMVar
          -- Server
          -- forkIO $ waitHomer homer_recv
          --   >>= (\x -> check x m)
          --  >> return ()

          -- Client
          -- forkIO $ delay 1000000 >> homerFlyWith homer_send l >> return ()

          r <- takeMVar m
          assertEqual "HomerTest" 't' r

        l = Letter (ident2Str $ Identity "item1" "12345")
            (fromList [("iid", "1")])
            (fromList [("T1", "T")])
        l1 = Letter (ident2Str $ Identity "item1" "12345")
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
