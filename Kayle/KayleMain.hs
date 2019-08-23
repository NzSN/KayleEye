-- Kayle, a program to do judgement of quality of code, accept or reject.

{-#LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment

-- Configuration
import Modules.ConfigReader
import Control.Monad.Reader

-- Constants
import KayleConst
import KayleBasics

import Homer

type Revision = String
type JudgeContent = String

main =
  let f = (\p ->
             -- Init Homer
              let serverOpts = configGet (snd p)  serverInfoGet serverAddr_err_msg
              in (pickHomer (addr serverOpts) (port serverOpts))
              >>= (\x ->
                     -- Testing
                     (judge $ Prelude.head $ configSearch (snd p) "Command")
                    >>= (\isPass -> executor x isPass (snd p))))
  -- Get arguments and configurations
  in getArgs >>= (\args -> loadConfig (Prelude.head args) configPath >>= (\config -> return (args, config))) >>= f

-- Accept if pass test otherwise throw an error
executor :: Homer -> Bool -> Configs -> IO ()
executor h True c = print "fixme"
executor h False c = error "Test failed"

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess
