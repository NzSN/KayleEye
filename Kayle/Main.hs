-- Kayle, a program to do judgement of quality of code, accept or reject.
-- Arguments : ProjectName, iid, SHA

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment
import Data.Map
import Data.Maybe
import Data.Either
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
doJudge' args configs = do
  let serverOpts = cGetServer configs
      testCmds = cGetCmds configs
      homer = pickHomer (addr serverOpts) (port serverOpts)

      judgeProc = judge testCmds (cmds args) False
      reJudge   = judge testCmds "@failed" True

      -- Testing
      -- If first test is failed then run the commands
      -- pair with @failed, this provide opportunity to
      -- do clean and try again.
      testing = judgeProc >>=
                  \judge' -> either (\_ -> reJudge) (\_ -> return $ Right True) judge'
                  >>= \x -> return $ fromRight False x
      ident_ = ident2Str $ Identity (proj args) (sha args) (event args)

      next h = do
        -- Do testing job
        success <- testing
        -- Send testing result
        notify h args success
        -- Terminated the testing
        terminatePhase' ident_ h args
        -- Throw error if testing is failed
        throwError success

  -- Get homer
  h <- homer
  -- Get seqId in prepare phase
  isAccepted <- preparePhase' h args

  if isAccepted
    then next h
    else return ()

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
      -> IO (Either Bool Bool)
judge TestContent_None bCmds isExclusive = return $ Right True
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
    loop [] controls = return $ Right True
    doNextOr b c ctrl =
      if b == False
      then return $ Left False
      else loop c ctrl
