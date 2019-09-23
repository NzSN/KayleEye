-- file: Homer.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Homer where

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
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Internal as BI
import Data.Text as Text
import Network.Socket.ByteString as BS

-- Identity Processing functions
type IdentStr = String
data Identity = Identity { ident_name :: String, ident_sha :: String }

emptyIdent :: Identity
emptyIdent = Identity "" ""

ident2Str :: Identity -> IdentStr
ident2Str i = (ident_name i) ++ ":" ++ (ident_sha i)

str2Ident :: IdentStr -> Identity
str2Ident str = Identity (Prelude.head identPart) (Prelude.last identPart)
  where identPart = identSplit str

identSplit :: String -> [String]
identSplit [] = [""]
identSplit all@(x:xs)
  | x == ':'   = "" : shaPart
  | otherwise = (x : (Prelude.head $ identSplit xs)) : Prelude.tail shaPart

  where shaPart = identSplit xs

-- Homer
data Homer = Homer { rSocket :: Socket, rAddr :: SockAddr }

-- Letter
data Letter = Letter { ident :: IdentStr,
                       header :: Map String String,
                       content :: Map String String } | Empty_letter deriving Show

instance ToJSON Letter where
  toJSON (Letter ident header content) =
    object ["ident" .= ident,
            "header" .= header,
            "content" .= content]

instance FromJSON Letter where
  parseJSON = withObject "Letter" $ \v -> Letter
    <$> v .: "ident"
    <*> v .: "header"
    <*> v .: "content"

sizeOfLetter :: Letter -> Int
sizeOfLetter l = size . content $ l

isEmptyLetter :: Letter -> Bool
isEmptyLetter Empty_letter = True
isEmptyLetter _ = False

emptyLetter :: Letter
emptyLetter = Letter "" Map.empty Map.empty

letterUpdate :: Letter
             -> String -- Key
             -> String -- New Value
             -> Letter
letterUpdate l k nv =
  Letter (ident l) (header l) (updateWithKey (\k_ v_ -> Just nv) k (content l))

letterUpdate' :: Letter
              -> [(String, String)]
              -> Letter
letterUpdate' l [] = l
letterUpdate' l (k:ks) = do
  letterUpdate' (letterUpdate l (fst k) (snd k)) ks

allKeysOfContent :: Letter -> [String]
allKeysOfContent l = keys $ content l

retriFromContent :: Letter
                 -> String -- Key
                 -> Maybe String
retriFromContent l k = Map.lookup k (content l)
retriFromHeader :: Letter
                -> String -- key
                -> Maybe String
retriFromHeader l k = Map.lookup k (header l)

-- Check letter content to see that is test pass
isTestFinished :: Letter -> Bool
isTestFinished l = let content_of_l = content l
                   in Prelude.foldl (\acc x -> acc && x) True
                      [ (fromJust $ Map.lookup k content_of_l) /= "O" | k <- allKeysOfContent l]

isTestSuccess :: Letter -> Bool
isTestSuccess l = let content_of_l = content l
                  in Prelude.foldl (\acc x -> acc && x) True
                     [ (fromJust $ Map.lookup k content_of_l) == "T" | k <- allKeysOfContent l]

pickHomer :: String -- HostName
        -> String -- Port
        -> IO Homer
pickHomer host port = do
    addrInfos <- getAddrInfo Nothing (Just host) (Just port)

    let serverAddr = Prelude.head addrInfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

    return $ Homer sock (addrAddress serverAddr)

pickHomer' :: String -> String -> IO Homer
pickHomer' host port = do
    addrInfos <- getAddrInfo Nothing (Just host) (Just port)

    let serverAddr = Prelude.head addrInfos
    sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
    bind sock (addrAddress serverAddr)

    return $ Homer sock (addrAddress serverAddr)


letterBuild :: Letter -> ByteString
letterBuild l = encode $ Letter (ident l) (header l) (content l)

homerFlyWith :: Homer -> Letter -> IO Int
homerFlyWith homer letter = do
  let letterStr = toStrict $ letterBuild letter
  BS.sendTo (rSocket homer) letterStr (rAddr homer)

waitHomer :: Homer -> IO Letter
waitHomer homer = do
  (rawLetter_, addr) <- BS.recvFrom (rSocket homer) 1024

  let letter = (decode $ fromStrict rawLetter_ :: Maybe Letter)

  if isNothing letter
    then return $ emptyLetter
    else return $ fromJust letter

-- Test cases

homerTest :: Test
homerTest = TestList [TestLabel "Send and recv" (TestCase homerAssert),
                      TestLabel "Letter update" (TestCase homerAssert1),
                      TestLabel "Letter updates" (TestCase homerAssert2),
                      TestLabel "Letter Header Retri" (TestCase homerAssert3)]
  where homerAssert = do
          homer_recv <- pickHomer' "localhost" "8011"
          homer_send <- pickHomer "localhost" "8011"

          m <- newEmptyMVar
          -- Server
          forkIO $ waitHomer homer_recv
            >>= (\x -> check x m)
            >> return ()

          -- Client
          forkIO $ delay 1000000 >> homerFlyWith homer_send l >> return ()

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
