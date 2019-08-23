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

import System.IO
import System.Directory
import System.Environment
import System.Process as Process

import Control.Concurrent.Thread.Delay

-- Email
import Network.Mail.SMTP

redirectToNull :: String
redirectToNull = " 1>/dev/null 2>/dev/null "

-- Load configuration file from gitlab and then parsing it
loadConfig :: String -> String -> IO Configs
loadConfig proj path = do
  -- Load configuration file from gitlab
  isSuccess <- run_command_1 ("git clone " ++ configPath ++ " config")
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
    parsingConfig :: IO [[String]]
    parsingConfig = do
      file_ref <- openFile ("./config/" ++ proj ++ ".txt") ReadMode
      contents <-  hGetContents file_ref

      let config = fromRight (("error":[]):[]) $ parseConfig contents
      return config

-- Accept an merge request into main branch
accept :: Manager -> Configs
       -> String -- iid, Merge request id
       -> IO ()
accept mng cfgs iid = do
  args <- getArgs

  let acceptUrl = replace_iid (Prelude.head $ configSearch cfgs "AcceptUrl") iid
  code <- put_req acceptUrl mng

  case code of
    -- If merge request is unable to be accepted (ie: Work in Progress,
    -- Closed, Pipeline Pending Completion, or Failed while requiring Success) -
    -- you’ll get a 405 and the error message ‘Method Not Allowed’
    405 -> notify "Merge request is unable to be accepted" cfgs
    -- If it has some conflicts and can not be merged - you’ll get a 406 and
    -- the error message ‘Branch cannot be merged’
    406 -> rebase mng cfgs iid >> delay (3 * seconds_micro) >> print "fixme"
    -- If the sha parameter is passed and does not match the HEAD of the source -
    -- you’ll get a 409 and the error message ‘SHA does not match HEAD of source branch’
    409 -> notify "SHA does not match HEAD of source branch" cfgs
    -- If you don’t have permissions to accept this merge request - you’ll get a 401
    401 -> notify "Permissions denied" cfgs
    -- Merge request not found or the url is wrong
    404 -> notify "Merge requests not found or the url is wrong" cfgs
    -- Success
    200 -> return ()

-- Rebase merge request to target branch
rebase :: Manager -> Configs
       -> String -- iid, Merge request id
       -> IO ()
rebase mng cfgs iid = do
  args <- getArgs
  let rebaseUrl = replace_iid (Prelude.head $ configSearch cfgs "RebaseUrl") iid
  code <- put_req rebaseUrl mng

  case code of
    -- If you don’t have permissions to push to the merge request’s source branch -
    -- you’ll get a 403 Forbidden response.
    403 -> notify "Permission denied to rebase merge request" cfgs
    -- The API will return a 202 Accepted response if the request is enqueued successfully
    -- Merge request not found or the url is wrong
    404 -> notify "Merge requests not found or the url is wrong" cfgs
    -- Success
    202 -> return ()

-- Put event to http server
put_req :: String -> Manager -> IO Int
put_req url mng = do
  initialRequest <- parseRequest url
  response <- httpLbs (initialRequest { method = "PUT" }) mng
  return $ statusCode $ responseStatus response

-- Notify via email
notify :: Lazy.Text -> Configs -> IO ()
notify content cfgs = do
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      hostName = Config.host mailInfo
      user = Config.user mailInfo
      pass = Config.pass mailInfo

      adminEmail = (cs $ adminEmailAddr addr) :: Internal.Text

  let from = Address Nothing ((cs user) :: Internal.Text)
      to   = [Address (Just "admin") adminEmail]
      cc   = []
      bcc  = []
      subject = "CI informations"
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
  isSuccess <- try (callCommand $ cmd ++ redirectToNull) :: IO (Either SomeException ())
  case isSuccess of
    Left ex -> return False
    Right () -> return True

-- Replace '*' character with merge request id
replace_iid :: String -> String -> String
replace_iid ('*':xs) iid = iid ++ replace_iid xs iid
replace_iid (x:xs) iid = x : replace_iid xs iid
replace_iid "" iid = ""

-- Search option from Configs
configSearch :: Configs -> String -> [String]
configSearch configs theConfig = case theConfig of
  "Command" -> case testCmdGet configs of
                 Nothing -> error errorMsg
                 Just cmd -> Config.content cmd
  "AcceptUrl" -> case mrAcceptApiConfig configs of
                   Nothing -> error errorMsg
                   Just url -> Config.a_api url : []
  "RebaseUrl" -> case mrRebaseApiConfig configs of
                   Nothing -> error errorMsg
                   Just url -> Config.r_api url : []
  where errorMsg = "No " ++ theConfig ++ " found"

configGet :: Configs -> (Configs -> Maybe a) -> String -> a
configGet cfgs f errMsg = case f cfgs of
                     Nothing  -> error errMsg
                     Just opt -> opt
