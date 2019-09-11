-- Kayle, a program to do judgement of quality of code, accept or reject.
-- Arguments : ProjectName, iid, SHA

{-#LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment
import Data.Map
import Data.Maybe
import Data.List
import Data.List.Split

-- Configuration
import Modules.ConfigReader as C
import Control.Monad.Reader

-- Constants
import qualified KayleConst as KConst
import KayleBasics hiding (notify)

import Homer

type Revision = String
type JudgeContent = String

data KayleArgs = KayleArgs {
  proj :: String,
  target :: String,
  sha :: String,
  iid :: String,
  configPath :: String,
  cmds :: String } deriving Show

getKayleArgs :: IO KayleArgs
getKayleArgs = do
  args <- getArgs
  return $ KayleArgs
    ((!!) args 0) -- proj
    ((!!) args 1) -- target
    ((!!) args 2) -- sha
    ((!!) args 3) -- iid
    ((!!) args 4) -- configPath
    ((!!) args 5) -- build cmds

main :: IO ()
main = let c args = (loadConfig cfile $ configPath args)
                    >>= (\config -> (print config) >> (return (args, config))) >>= f
             where cfile = (proj args) ++ "_" ++ (target args)

           f p = let serverOpts = configGet (snd p) serverInfoGet KConst.serverAddr_err_msg
                     testCmds = configGet (snd p) testCmdGet KConst.test_cmd_err_msg
                     -- Testing
                     testing h = (judge testCmds (cmds (fst p)) )
                                 >>= (\isPass -> (notify h (fst p) isPass) >> (throwError isPass))
                 in pickHomer (addr serverOpts) (port serverOpts) >>= testing
  -- Get arguments and configurations
       in getKayleArgs >>= c
  where throwError bool = if bool == False then error "Test failed" else return ()
-- Accept if pass test otherwise throw an error
notify :: Homer -> KayleArgs -> Bool -> IO ()
notify h args False = notify' h args (fromList [(target args, "F")])
notify h args True = notify' h args (fromList [(target args, "T")])

notify' :: Homer -> KayleArgs -> Map String String -> IO ()
notify' homer args c = let i = ident2Str $ Identity (proj args) (sha args)
                           h = fromList [("iid", (iid args))]
                           l = Letter i h c
                       in homerFlyWith homer l >> return ()

judge :: TestContent_cfg
      -> String -- Build commands
      -> IO Bool
judge TestContent_None bCmds = return True
judge c bCmds = do
  let cmds = C.content c
      controls = splitOn " " bCmds
  -- "@" Which is default control command for a test command
  -- it means the test command can be run.
  loop cmds $ ("@":controls)
  where
    loop (cmd:cmds) controls = do
      let control_str = fst cmd
          cmd_str = snd cmd
      if elem control_str controls
        then (run_command_1 cmd_str) >>= (\x -> doNextOr x cmds controls)
        else doNextOr True cmds controls
    loop [] controls = return True
    doNextOr b c ctrl =
      if b == False
      then return False
      else loop c ctrl
