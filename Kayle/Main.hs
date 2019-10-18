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
import Control.Monad.Trans.State.Lazy
import Control.Exception as Ex
import Control.Concurrent.Thread.Delay

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
       in getKayleArgs
          >>= beginToJudge
          >> return ()

  where cfileName p t = p ++ "_" ++ t

doJudge' :: KayleArgs -> Configs -> IO ()
doJudge' args configs = do
  let testCmds = cGetCmds configs

      ident_ = ident2Str $ Identity (proj args) (sha args) (event args)

      judgeProc = judge testCmds (cmds args) False
      reJudge   = judge testCmds "@failed" True

      -- Testing
      -- If first test is failed then run the commands
      -- pair with @failed, this provide opportunity to
      -- do clean and try again.
      testing = judgeProc
                >>= either (\_ -> reJudge) (\_ -> return $ Right True)
                >>= return . fromRight False

      process = testing

      post h success = do
        -- Send testing result
        (h1, _) <- solidTunnel h $ \homer -> notify homer args success
        -- Terminated the testing
        (h2, _) <- solidTunnel h1 $ \homer -> terminatePhase' ident_ homer args

        releaseHomer h2

        -- Throw error if testing is failed
        throwError success

  -- Get homer
  h <- gHomerUntil addr_ port_

  -- Have a request to KayleHome
  isAccepted <- Ex.handle (\(SomeException e) -> doJudge' args configs >> return False) (preparePhase' h args)

  if isAccepted
    then process
         >>= post h
    else fail "Request is rejected"

  where
    serverOpts = cGetServer configs
    addr_ = addr serverOpts
    port_ = port serverOpts
    homer = pickHomer (addr serverOpts) (port serverOpts)

    -- Throw if judge failed
    throwError bool = if bool == False then error "Test failed" else return ()

    solidTunnel :: Homer -> (Homer -> IO a) -> IO (Homer, a)
    solidTunnel h f = do
      Ex.handle (\e -> print "Solid Tunnel" >> solidHandler f e) $ (f h >>= \a -> return (h,a))

    solidHandler f (SomeException e) = do
      -- Reconnect to KayleHome After connection builded just send request
      h <- prepareUntil args
      -- If the request is rejected just throw an exception
      if isNothing h
        then fail "Request is rejected"
        else solidTunnel (fromJust h) f

    prepareUntil args = do
      h <- gHomerUntil addr_ port_
      h1 <- Ex.handle (\(SomeException e) -> (delay $ 10 * KConst.seconds_micro) >> prepareUntil args)
            $ (preparePhase' h args
               >>= \isAccepted -> if isAccepted
                                  then return $ Just $ h
                                  else return $ Nothing)
      return h1

    gHomerUntil addr port =
      Ex.handle (\(SomeException e) -> (delay $ 10 * KConst.seconds_micro)
                  >> gHomerUntil addr port) homer

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
