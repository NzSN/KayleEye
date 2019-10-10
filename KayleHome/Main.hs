-- KayleHome Collect judgements of kayle and then execute if these judgements is right.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Data.String.Conversions (cs)

-- Http Request
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

-- Process, File, Directory
import System.Environment

-- Configuration
import Modules.ConfigReader as C

-- Monad Transformers
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

-- Socket
import Network.Socket

-- Map
import Data.Map as Map
import Data.Map.Merge.Strict

-- List
import Data.List as List

import Control.Monad.Writer

-- Maybe
import Data.Maybe

-- Json
import Data.Aeson

-- Database
import Database.HDBC

-- Homer
import Letter as L
import Homer as H
import LetterBox
import KayleConst
import Logger
import KayleBasics as K
import RepoOps as R
import Actions
import KayleDefined
import Puller
import Room

import Control.Concurrent

import System.Systemd.Daemon

type Kayle = ReaderT KayleEnv (LoggerT IO) Integer

runKayle :: Kayle -> KayleEnv -> IO ()
runKayle k e =
  runLoggerT (runReaderT k e) >> return ()

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) (head . tail $ args)
  print configs
  let serverOpts = configGet configs serverInfoGet serverAddr_err_msg
  homer <- pickHomer (C.addr serverOpts) (C.port serverOpts)

  -- Box initialization
  let dbOpts = configGet configs databaseGet db_err_msg
  bKey <- boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts)

  -- Database init, Create procTbl and historyTbl if not exists
  boxInit bKey

  room <- newRoom
  -- This locker is shared between puller and KayleHome
  locker <- newEmptyMVar

  let env = (KayleEnv configs manager args homer bKey room locker Map.empty)
  runKayle doKayle env

-- Append log message to Kayle
logKayle h = lift . appendLogger h

waitKey_until :: Configs -> IO BoxKey
waitKey_until c =
  let dbOpts = configGet c databaseGet db_err_msg
  in catchSql (boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts))
     (\_ -> (threadDelay boxKeyRetryInterval) >> waitKey_until c)

doKayle :: Kayle
doKayle =
  ask >>= \l -> procLoop Empty_letter l
  where
    procLoop :: Letter -> KayleEnv -> Kayle
    procLoop l env = do
      let homer = envHomer env
          bKey = envKey env
          room = envRoom env

      letter <- if isEmptyLetter l
                then liftIO . getLetter $ room
                else return l

      -- Deal with control letter
      env_ <- if typeOfLetter letter == control_event
              then liftIO . controlProc letter $ env
              else return env

      -- fixme: should provide function to deal with different error type
      eType <- liftIO . catchSql (procLetter' letter bKey env_) $ procHandler

      case eType of
        -- OK
        0 -> procLoop Empty_letter env_
        -- ERROR, just retry the letter with new box key.
        1 -> (liftIO . waitKey_until $ (envCfg env_))
          >>= \x -> let env_new = KayleEnv
                                  (envCfg env_) (envMng env_)
                                  (envArgs env_) (envHomer env_) x
                                  (envRoom env_) (envPullerLocker env_)
                                  (envControl env_)
                    in procLoop l env_new

    procHandler = \_ -> return k_error :: IO Integer
    procLetter' l b e =
      doLogger (runReaderT (procLetter l b e) e) (last $ envArgs e)
      >>= \retCode -> commitKey b >> return retCode

    -- Function to process new incomming letter
    procLetter :: Letter -> BoxKey -> KayleEnv -> Kayle
    procLetter letter bKey env = do
      logKayle "Info" $ "Received Letter : " ++ (show letter)
      exists <- liftIO . isLetterExists bKey historyTbl $ (ident letter)
      if not exists
        then (liftIO . isLetterExists bKey procTbl $ (ident letter))
             >>= \exists -> if not exists
                            then newLetter letter env
                            else inProcLetter letter env
        else action' True letter $ env

    -- Function to deal with the first arrived letter of a project
    newLetter :: Letter -> KayleEnv -> Kayle
    newLetter l env =
        let cfgs = envCfg env
            bKey = envKey env
        in (return $ letterInit cfgs l)
            >>= \x -> if isNothing x
                    then logKayle "Error" "letterInit failed"
                    else newLetterProc l (fromJust x) env bKey

    -- Function to deal with situation there is only one test content in config
    newLetterProc :: Letter -- Letter generate by letterInit
                    -> Letter -- Letter is received from KayleEye
                    -> KayleEnv
                    -> BoxKey
                    -> Kayle
    newLetterProc rl l e b = if sizeOfLetter l == 1
                        then newLetterInsert l historyTbl b >>
                                if isTestSuccess l
                                then action True rl e
                                else action False rl e
                        else newLetterInsert l procTbl b

    -- Function to insert new letter into table
    newLetterInsert :: Letter -> String -> BoxKey -> Kayle
    newLetterInsert l tbl k =
        (logKayle "Info" $ "Insert letter : " ++ (show l) ++ " Into " ++ tbl)
        >> (liftIO . insertLetter k tbl) l
        >> return k_ok

    -- Function to process inProc letter
    inProcLetter :: Letter -> KayleEnv -> Kayle
    inProcLetter l env =
        let cfgs = envCfg env
            bKey = envKey env
        in (liftIO . runMaybeT $ searchLetter bKey procTbl (ident l))
            >>= (\x -> if isNothing x
                    then logKayle "Warning" "Letter doesn't exists"
                    else inProcDo l (fromJust x) env)

    -- Function to update received letter into box
    inProcDo :: Letter -- The recevied letter
                -> Letter -- The letter from box
                -> KayleEnv -> Kayle
    inProcDo rl bl env =
        let content = L.content rl
            -- Update the letter from box
        in (return $ letterUpdate' bl [(k, fromJust $ Map.lookup k content) | k <- allKeysOfContent rl])
            -- Put the letter from box back to box
            >>= (\x -> logKayle "Info" ("Update letter : " ++ (show x)) >>
                    (liftIO . updateLetter (envKey env) (L.ident x) (encode $ L.content x) $ isTestFinished x))
            -- To check that whether the test describe by the letter is done
            >>= (\x -> if x == 1
                         then if isTestSuccess rl
                           then logKayle "Info" "Accpet Letter" >> action True rl env
                           else action False rl env
                         else return k_ok)

controlProc :: Letter -> KayleEnv -> IO KayleEnv
controlProc l env =
  let cmd = fromJust $ retriFromContent l "cmd"
      proc = case cmd of
               "noMerged" -> noMergedCmd
               "terminated" -> terminatedCmd
  in proc l env

noMergedCmd :: Letter -> KayleEnv -> IO KayleEnv
noMergedCmd l env = do
  let cEnv = envControl env
      subEnv_May = subCtrlEnv cEnv cmd_noMerged
      proj = head $ identSplit (ident l)
      locker = envPullerLocker env

  -- The Command's subEnv must exists
  cEnv_new <- fromJust $
    (\subEnv ->
        let item = ctrlItem subEnv proj
        in if isNothing item
           then let new_env = itemInsert cEnv cmd_noMerged proj ["locked"]
                in putMVar locker () >> return new_env
           else return cEnv)
    <$> subEnv_May

  return $ kayleEnvSetCEnv env cEnv_new

terminatedCmd :: Letter -> KayleEnv -> IO KayleEnv
terminatedCmd l env = do
  let cEnv = envControl env
      subEnv_May = subCtrlEnv cEnv cmd_terminated
      proj = head $ identSplit (ident l)
      subTest = fromJust $ retriFromContent l "target"
      allSubTest = fromJust $ Map.lookup proj (testContent $ fromJust $ testPiecesGet proj (envCfg env))

  -- The Command's subEnv must exists
  cEnv_new <- fromJust $
    (\subEnv ->
       let item = ctrlItem subEnv proj
       in if isNothing item
          then let new_env = itemInsert cEnv cmd_terminated proj [subTest]
               in if isAllTerminated [subTest] allSubTest
                  then return $ clean cEnv proj
                  else return cEnv
          else let subTests = subTest:(fromJust item)
                   new_env = itemInsert cEnv cmd_terminated proj subTests
               in if isAllTerminated subTests allSubTest
                  then return $ clean cEnv proj
                  else return cEnv)
    <$> subEnv_May

  let termEnv = fromJust $ subCtrlEnv cEnv_new cmd_terminated
      locker = envPullerLocker env

  if Map.size termEnv > 0
    then takeMVar locker
    else return ()

  return $ kayleEnvSetCEnv env cEnv_new

  where
    isAllTerminated :: [String] -> [String] -> Bool
    isAllTerminated subT allT = List.foldl (\acc x -> acc && elem x subT) True allT

    clean :: CtrlEnv -> String -> CtrlEnv
    clean cEnv proj = itemDelete cEnv cmd_terminated (head $ identSplit (ident l))

-- Action be perform after test project done
action :: Bool -> Letter -> KayleEnv -> Kayle
action success l env =
  let event = fromJust $ retriFromHeader l "event"
  in liftIO . actionSelector event success l $ env
action' :: Bool -> Letter -> KayleEnv -> Kayle
action' success l env =
  let event = fromJust $ retriFromHeader l "event"
  in liftIO . actionSelector' event success l $ env
