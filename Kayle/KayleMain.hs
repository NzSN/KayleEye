-- Kayle, a program to do judgement of quality of code, accept or reject.
-- Arguments : ProjectName, iid, SHA

{-#LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment
import Data.Map

-- Configuration
import Modules.ConfigReader
import Control.Monad.Reader

-- Constants
import KayleConst
import KayleBasics hiding (notify)

import Homer

type Revision = String
type JudgeContent = String

main :: IO ()
main = let c args = (loadConfig (head args) configPath)
                    >>= (\config -> return (args, config)) >>= f
           f p = let serverOpts = configGet (snd p) serverInfoGet serverAddr_err_msg
                     -- Testing
                     testing h = (judge $ head $ configSearch (snd p) "Command")
                                 >>= (\isPass -> notify h (fst p) isPass)
                 in pickHomer (addr serverOpts) (port serverOpts) >>= testing
  -- Get arguments and configurations
       in getArgs >>= c

-- Accept if pass test otherwise throw an error
notify :: Homer -> [String] -> Bool -> IO ()
notify h args False = notify' h args (fromList [(head args, "F")])
notify h args True = notify' h args (fromList [(head args, "T")])

notify' :: Homer -> [String] -> Map String String -> IO ()
notify' homer args c = let i = ident2Str $ Identity (head args) (last args)
                           h = fromList [("iid", (head . tail $ args))]
                           c = fromList [(head args, "T")]
                           l = Letter i h c
                       in homerFlyWith homer l >> return ()

judge :: JudgeContent -> IO Bool
judge c = do
  isSuccess <- run_command_1 c
  return isSuccess
