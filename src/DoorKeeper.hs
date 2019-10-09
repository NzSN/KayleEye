-- file: DoorKeeper

module DoorKeeper where

import Letter
import Homer
import Room
import KayleDefined
import KayleConst
import LetterBox
import Modules.ConfigReader

import Data.Maybe
import Data.Map as Map
import Network.Socket
import Control.Monad
import Control.Concurrent

doorKeeper :: KayleEnv -> IO ()
doorKeeper env = do
  let cfgs = envCfg env
      room = envRoom env
  masterSock <- listenSockGet cfgs

  forever $ doorProc masterSock

  where
    doorProc sock = waitHomer sock
      >>= \h -> forkIO $ doorKeeperWork h env

doorKeeper' :: Homer -> KayleArgs -> IO ()
doorKeeper' h args = do
  let event_ = event args
      i = ident2Str $ Identity (proj args) (sha args) (event args)
      header = fromList [("event", "daily")]
  if event_ == "daily"
    then sendLetter h (Letter i header Map.empty) >> return ()
    else return ()

doorKeeperWork :: Homer -> KayleEnv -> IO ()
doorKeeperWork h env = do
  -- Prepare stage: Is letter already been processed ? need sync ?
  preparePhase h env
  -- Test result collected
  collectPhase h env

preparePhase :: Homer -> KayleEnv -> IO ()
preparePhase h env = do
  -- Waiting for the first ask letter
  l_ask <- waitLetter h

  let event_ = retriFromHeader l_ask "event"
      preFunc e = case e of
                  "merge_request" -> mergePrepare
                  "push"          -> pushPrepare
                  "daily"         -> dailyPrepare

  if isNothing event_
    then return ()
    else (preFunc $ fromJust event_) l_ask h env

mergePrepare :: Letter -> Homer -> KayleEnv -> IO ()
mergePrepare l h env = return ()

pushPrepare :: Letter -> Homer -> KayleEnv -> IO ()
pushPrepare l h env = return ()

-- Sync: All test of a daily test should carry on the same revision
dailyPrepare :: Letter -> Homer -> KayleEnv -> IO ()
dailyPrepare l h env =
  let header = fromList [("event", control_event)]
      content = fromList [("cmd", cmd_noMerged)]
  in putLetter (envRoom env) (Letter (ident l) header content )

collectPhase :: Homer -> KayleEnv -> IO ()
collectPhase h env = do
  l <- waitLetter h

  if typeOfLetter l == disconn_event
    then releaseHomer h
    -- put push,merge,daily letter into room
    else putLetter (envRoom env) l >> collectPhase h env

listenSockGet :: Configs -> IO Socket
listenSockGet cfgs = do
  let serverOpts = configGet cfgs serverInfoGet serverAddr_err_msg

  (serverAddr:xs) <- getAddrInfo Nothing (Just $ addr serverOpts) (Just $ port serverOpts)

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 10

  return sock
