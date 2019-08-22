-- Kayle, a program to do judgement of quality of code, accept or reject.

{-#LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment

-- Configuration
import Modules.ConfigReader

-- Constants
import KayleConst
import KayleBasics

type Revision = String
type JudgeContent = String

main = do

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath

  isPass <- judge $ Prelude.head $ configSearch configs "Command"
  -- Accept or reject depend on judge result
  executor isPass configs


-- Accept if pass test otherwise throw an error
executor :: Bool -> Configs -> IO ()
executor True c = print "fixme"
executor False c = error "Test failed"

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess

-- Accept, this function will notify kayleHome the part test
-- current instance is pass
