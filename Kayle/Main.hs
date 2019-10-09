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

import Letter
import Homer
import DoorKeeper
import KayleDefined

type Revision = String
type JudgeContent = String

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
    ((!!) args 6) -- isMr

isMr' :: String -> Bool
isMr' s = s == KConst.mr_event

main :: IO ()
main = let beginToJudge args =
             (loadConfig (cfileName (proj args) (target args)) $ configPath args)
             >>= \configs -> doJudge' args configs
          -- Get arguments and configurations
       in getKayleArgs >>= beginToJudge

  where cfileName p t = p ++ "_" ++ t

doJudge' :: KayleArgs -> Configs -> IO ()
doJudge' args configs =
  let serverOpts = cGetServer configs
      testCmds = cGetCmds configs
      -- Testing
      testing h = judge testCmds (cmds args) False
                  -- If first test is failed then run the commands
                  -- pair with @failed, this provide opportunity to
                  -- do clean and try again.
                  >>= (\x -> if x == False
                            then judge testCmds "@failed" True
                            else return True)
                  >>= \x -> (notify h args x) >> throwError x
  in pickHomer (addr serverOpts) (port serverOpts)
     >>= \h -> doorKeeper' h args >> testing h

  where
    -- Throw if judge failed
    throwError bool = if bool == False then error "Test failed" else return ()

-- Accept if pass test otherwise throw an error
notify :: Homer -> KayleArgs -> Bool -> IO ()
notify h args False = notify' h args (fromList [(target args, "F")])
notify h args True = notify' h args (fromList [(target args, "T")])

notify' :: Homer -> KayleArgs -> Map String String -> IO ()
notify' homer args c = let i = ident2Str $ Identity (proj args) (sha args) (event args)
                           h = fromList [("event", event args), ("iid", iid args)]
                           l = Letter i h c
                       in sendLetter homer l >> return ()

judge :: TestContent_cfg
      -> String -- Build commands
      -> Bool   -- Is in exclusive mode
      -> IO Bool
judge TestContent_None bCmds isExclusive = return True
judge c bCmds isExclusive = do
  let cmds = C.content c
      controls = splitOn " " bCmds

  if isExclusive
    then loop cmds $ controls
    -- "@" Which is default control command for a test command
    -- it means the test command can be run.
    else loop cmds $ ("@":controls)
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
