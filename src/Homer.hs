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
                       content :: Map String String }

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

emptyLetter :: Letter
emptyLetter = Letter "" Map.empty Map.empty

letterUpdate :: Letter
             -> String -- Key
             -> String -- New Value
             -> Letter
letterUpdate l k nv =
  Letter (ident l) (header l) (updateWithKey (\k_ v_ -> Just nv) k (content l))

letterUpdate' :: Letter
              -> [String] -- Keys
              -> [String] -- New Valuse
              -> Letter
letterUpdate' l [] [] = l
letterUpdate' l kk@(k:ks) vv@(v:vs) =
  if not $ List.length kk == List.length vv
  then emptyLetter
  else letterUpdate' (letterUpdate l k v) ks vs

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

letterBuild :: Letter -> ByteString
letterBuild l = encode $ Letter (ident l) (header l) (content l)

homerFlyWith :: Homer -> Letter -> IO ()
homerFlyWith homer letter =
  BS.sendTo (rSocket homer) (toStrict $ letterBuild letter) (rAddr homer) >> return ()

waitHomer :: Homer -> IO Letter
waitHomer homer = do
  (rawLetter_, addr) <- BS.recvFrom (rSocket homer) 1024

  let letter = (decode $ fromStrict rawLetter_ :: Maybe Letter)
  if isNothing letter
    then return $ emptyLetter
    else return $ fromJust letter
