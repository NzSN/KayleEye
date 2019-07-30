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

-- Process, File, Directory
import System.Process as Process
import Control.Exception
import System.IO
import System.Directory
import System.Environment

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

type Revision = String
type JudgeContent = String
type Configs = [[String]]

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath
  print configs
  print $ testCmdGet configs

  -- Do judgement
  isPass <- judge "make"

  if isPass == True
    then return executor True
    else return executor False

-- For Success case we just accept it otherwise
-- send an email to to person who request the case
executor :: Bool -> Configs -> IO Bool
executor isPass cfgs =
  if isPass == True
    then trueAction cfgs
    else falseAction cfgs

  where
    trueAction :: Configs -> IO Bool
    trueAction cfgs = do

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess

-- Load configuration file from gitlab and then parsing it
loadConfig :: String -> String -> IO Configs
loadConfig proj path = do
  -- Load configuration file from gitlab
  isSuccess <- run_command_1 ("git clone " ++ configPath)
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
