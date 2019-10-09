-- file: KayleBasics.hs

{-# LANGUAGE OverloadedStrings #-}

module KayleBasics where

import KayleConst
import Modules.ConfigReader as Config

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Data.Either
import Data.Maybe

import Data.String.Conversions (cs)
import Data.Text.Internal.Lazy as Lazy (Text)
import Data.Text.Internal as Internal

import Control.Exception.Base

import qualified Data.ByteString.Lazy as BL

import System.IO
import System.Directory
import System.Environment
import System.Process as Process

import Control.Concurrent.Thread.Delay

import Letter as L
import Homer as H
import Data.Map as Map

-- Email
import Network.Mail.SMTP

redirectToNull :: String
redirectToNull = " 1>/dev/null 2>/dev/null "

-- Generate a letter via exists letter and configuration
letterInit :: Configs -> Letter -> Maybe Letter
letterInit cfgs l = do
  let letter_ident = ident_name . str2Ident . ident $ l
      workContent_m = Map.lookup letter_ident tContents
  if isNothing $ workContent_m
    then Nothing
    else let workContent = fromJust workContent_m
         in return $ Letter (ident l) (header l) $ fromList
            [ letterContentItem x (L.content l) |  x <- workContent ]
  where
    -- Contents of all testing projects
    tProjs = configGet cfgs (testPiecesGet (ident l)) test_proj_err_msg
    tContents = testContent tProjs
    -- All keys of content of letter
    allKeys = keys $ L.content l
    -- Item generator for content of new letter
    letterContentItem key content =
        if elem key allKeys
        then (key, lookup' key content)
        else (key, "O")
    lookup' k m = case Map.lookup k m of
                    Nothing -> "O"
                    Just v  -> v


-- Load configuration file from gitlab and then parsing it
loadConfig :: String -> String -> IO Configs
loadConfig proj path = do
  -- Load configuration file from gitlab
  isSuccess <- run_command_1 ("git clone " ++ path ++ " config")
  if isSuccess == True
    -- Parsing the configuration file specified by project
    then parsingConfig
    -- Otherwise update the configuration before parsing it
    else setCurrentDirectory "./config" >>
         run_command_1 ("git pull") >>
         setCurrentDirectory ".." >>
         -- Parsing
         parsingConfig
  where
    parsingConfig :: IO Configs
    parsingConfig = do
      file_ref <- openFile ("./config/" ++ proj ++ ".txt") ReadMode
      contents <-  hGetContents file_ref

      let config = fromRight Configs_Empty $ parseConfig contents
      return config

-- Put event to http server
put_req :: String -> Manager -> IO Int
put_req url mng = do
  initialRequest <- parseRequest url
  response <- httpLbs (initialRequest { method = "PUT" }) mng
  return $ statusCode $ responseStatus response

get_req :: String -> Manager -> IO (Int, BL.ByteString)
get_req url mng = do
  initialRequest <- parseRequest url
  response <- httpLbs initialRequest mng
  return $ (statusCode $ responseStatus response, (cs $ responseBody response))


-- Notify via email
-- fixme: pass user as argument of this function
notify :: Lazy.Text -> String -> Configs -> IO ()
notify content sub cfgs = do
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      hostName = Config.host mailInfo
      user = Config.user mailInfo
      pass = Config.pass mailInfo

      adminEmail = (cs $ adminEmailAddr addr) :: Internal.Text

  let from = Address Nothing ((cs user) :: Internal.Text)
      to   = [Address (Just "admin") adminEmail]
      cc   = []
      bcc  = []
      subject = (cs sub)
      body    = plainTextPart content

  let mail = simpleMail from to cc bcc subject [body]
  sendMailWithLogin hostName user pass mail

  where addr = case adminEmailGet $ cfgs of
          Nothing   -> error "Admin email address not found in configuration file"
          Just addr -> addr

-- Run shell command with args
run_command :: String -> [String] -> IO Bool
run_command cmd args = do
  status <- try (callProcess cmd args) :: IO (Either SomeException ())
  case status of
    Left ex -> return False
    Right () -> return True

-- Run shell command (args is within the string of shell command)
run_command_1 :: String -> IO Bool
run_command_1 cmd = do
  isSuccess <- try (callCommand $ cmd) :: IO (Either SomeException ())
  case isSuccess of
    Left ex -> return False
    Right () -> return True

-- Replace '*' character with merge request id
replace_iid :: String -> String -> String
replace_iid ('*':xs) iid = iid ++ replace_iid xs iid
replace_iid (x:xs) iid = x : replace_iid xs iid
replace_iid "" iid = ""

replaceAll :: String -> [String] -> String
replaceAll ('*':xs) (y:ys) = y ++ replaceAll xs ys
replaceAll (x:xs) str@(y:ys) = x:replaceAll xs str
replaceAll (x:xs) [] = x:replaceAll xs []
replaceAll "" (y:ys) = ""
replaceAll "" [] = ""

-- Search option from Configs
