-- file: Homer.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Homer where

-- Json
import Data.Aeson
import Data.Map
import Data.Maybe

import GHC.Generics

-- Socket
import Network.Socket
import System.IO
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Internal as BI
import Data.Text as Text
import Network.Socket.ByteString as BS

data Homer = Homer { rSocket :: Socket, rAddr :: SockAddr }

data Letter = Letter {
  proj :: String,
  content :: String,
  status :: String,
  ident :: String } deriving Show

instance ToJSON Letter where
  toJSON (Letter proj content status ident) =
    object ["proj" .= proj, "content" .= content, "status" .= status, "ident" .= ident ]

instance FromJSON Letter where
  parseJSON = withObject "Letter" $ \v -> Letter
    <$> v .: "proj"
    <*> v .: "content"
    <*> v .: "status"
    <*> v .: "ident"

emptyLetter :: Letter
emptyLetter = Letter "" "" "" ""

pickHomer :: String -- HostName
        -> String -- Port
        -> IO Homer
pickHomer host port = do
  addrInfos <- getAddrInfo Nothing (Just host) (Just port)

  let serverAddr = Prelude.head addrInfos
  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
  setSocketOption sock KeepAlive 1

  return $ Homer sock (addrAddress serverAddr)

letterBuild :: Letter -> ByteString
letterBuild l = encode $ Letter (proj l) (content l) (status l) (ident l)

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
