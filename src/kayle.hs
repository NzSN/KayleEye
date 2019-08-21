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
import Types

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

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess

-- Accept, this function will notify kayleHome the part test
-- current instance is pass


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
