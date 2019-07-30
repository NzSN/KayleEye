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
import Data.List.Split
import Data.Either

-- Process, File
import System.Process as Process
import Control.Exception
import System.IO
import System.Environment

-- Configuration
import Modules.ConfigReader

-- Constants

-- configPath is a url point to gitlab where to store
-- configuration files.
configPath :: String
configPath = "."

type Revision = String
type JudgeContent = String

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  -- There should only one member in args, project name
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath

  -- Do judgement
  isPass <- judge "make"
  if isPass == True
    then return executor True
    else return executor False

-- For Success case we just accept it otherwise
-- send an email to to person who request the case
executor :: Bool -> IO Bool

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess

-- Load configuration file from gitlab and then parsing it
loadConfig :: String -> String -> IO [[String]]
loadConfig proj path = do
  -- Load configuration file from gitlab
  isSuccess <- run_command_1 ("git clone " ++ configPath)
  -- Parsing the configuration file specified by project
  file_ref <- openFile ("./config/" ++ proj) ReadMode
  contents <-  hGetContents file_ref

  let config = fromRight (("error":[]):[]) $ parseConfig contents
  return config

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
