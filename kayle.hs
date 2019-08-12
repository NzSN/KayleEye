-- Kayle, a program to do judgement of quality of code, accept or reject.

{-#LANGUAGE OverloadedStrings #-}

import Debug.Trace

-- Http Request
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

-- Json
import Data.Aeson as Aeson
import Data.Maybe
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Either
import Data.Text.Internal.Lazy as Lazy (Text)
import Data.Text.Internal as Internal
import Data.String.Conversions (cs)

-- Process, File, Directory
import System.Process as Process
import Control.Exception
import System.IO
import System.Directory
import System.Environment
import Control.Concurrent.Thread.Delay

-- Email
import Network.Mail.SMTP

-- Exception
import Control.Exception.Base
import System.IO.Error

-- Configuration
import Modules.ConfigReader

-- Constants

-- configPath is a url point to gitlab where to store
-- configuration files.
configPath :: String
configPath = "git@gpon.git.com:root/CI_Config.git"

-- Unit is microseconds
seconds_micro :: Integer
seconds_micro = 1000000

type Revision = String
type JudgeContent = String
type Configs = [[String]]

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath

  -- Checking that is the configurations file be choosen is correct
  let isCorrect = isProjExists (Prelude.head args) configs

  if isCorrect
    -- Do judgement
    then do isPass <- judge $ Prelude.head $ configSearch configs "Command"

            -- Accept merge request if pass test otherwise exit with non-zero value.
            executor isPass manager configs

    else error "Incorrect configuration file"

-- Accept if pass test otherwise throw an error
executor :: Bool -> Manager -> Configs -> IO ()
executor True m c = accept m c
executor False m c = error "Test failed"

accept :: Manager -> Configs -> IO ()
accept mng cfgs = do
  args <- getArgs

  let acceptUrl = replace_iid (Prelude.head $ configSearch cfgs "AcceptUrl") (Prelude.last args)
  code <- put_req acceptUrl mng

  case code of
    -- If merge request is unable to be accepted (ie: Work in Progress,
    -- Closed, Pipeline Pending Completion, or Failed while requiring Success) -
    -- you’ll get a 405 and the error message ‘Method Not Allowed’
    405 -> notify "Merge request is unable to be accepted" cfgs
    -- If it has some conflicts and can not be merged - you’ll get a 406 and
    -- the error message ‘Branch cannot be merged’
    406 -> rebase mng cfgs >> delay (3 * seconds_micro) >> accept mng cfgs
    -- If the sha parameter is passed and does not match the HEAD of the source -
    -- you’ll get a 409 and the error message ‘SHA does not match HEAD of source branch’
    409 -> notify "SHA does not match HEAD of source branch" cfgs
    -- If you don’t have permissions to accept this merge request - you’ll get a 401
    401 -> notify "Permissions denied" cfgs
    -- Merge request not found or the url is wrong
    404 -> notify "Merge requests not found or the url is wrong" cfgs
    -- Success
    200 -> return ()

rebase :: Manager -> Configs -> IO ()
rebase mng cfgs = do
  args <- getArgs
  let rebaseUrl = replace_iid (Prelude.head $ configSearch cfgs "RebaseUrl") (Prelude.last args)
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

put_req :: String -> Manager -> IO Int
put_req url mng = do
  initialRequest <- parseRequest url
  response <- httpLbs (initialRequest { method = "PUT" }) mng
  return $ statusCode $ responseStatus response

notify :: Lazy.Text -> Configs -> IO ()
notify content cfgs = do
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      host = Prelude.head $ mailInfo
      user = Prelude.head . Prelude.tail $ mailInfo
      pass = Prelude.last $ mailInfo

      adminEmail = (cs $ fromJust $ adminEmailGet $ cfgs) :: Internal.Text

  let from = Address Nothing ((cs user) :: Internal.Text)
      to   = [Address (Just "admin") adminEmail]
      cc   = []
      bcc  = []
      subject = "CI informations"
      body    = plainTextPart content

  let mail = simpleMail from to cc bcc subject [body]
  sendMailWithLogin host user pass mail

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess

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

configSearch :: Configs -> String -> [String]
configSearch configs theConfig = case theConfig of
  "Command" -> case testCmdGet configs of
                 Nothing -> error errorMsg
                 Just cmd -> cmd:[]
  "AcceptUrl" -> case mrAcceptApiConfig configs of
                   Nothing -> error errorMsg
                   Just url -> url:[]
  "RebaseUrl" -> case mrRebaseApiConfig configs of
                   Nothing -> error errorMsg
                   Just url -> url:[]
  where errorMsg = "No " ++ theConfig ++ " found"

run_command :: String -> [String] -> IO Bool
run_command cmd args = do
  status <- try (callProcess cmd args) :: IO (Either SomeException ())
  case status of
    Left ex -> return False
    Right () -> return True

run_command_1 :: String -> IO Bool
run_command_1 cmd = do
  isSuccess <- try (callCommand cmd) :: IO (Either SomeException ())
  case isSuccess of
    Left ex -> return False
    Right () -> return True

replace_iid :: String -> String -> String
replace_iid ('*':xs) iid = iid ++ replace_iid xs iid
replace_iid (x:xs) iid = x : replace_iid xs iid
replace_iid "" iid = ""
