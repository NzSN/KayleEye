-- KayleHome Collect judgements of kayle and then execute if these judgements is right.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

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

-- Homer
import Homer as H
import LetterBox
import KayleConst
import Logger
import KayleBasics as K

import Control.Concurrent

type Args = [String]
data KayleEnv = KayleEnv { envCfg :: Configs,
                           envMng :: Manager,
                           envArgs :: Args,
                           envHomer :: Homer,
                           envKey :: BoxKey }

type Kayle = ReaderT KayleEnv (LoggerT IO) ()

runKayle :: Kayle -> KayleEnv -> IO ()
runKayle k e =
  (doLogger (runReaderT k e) (last $ envArgs e))
  -- loop to deal with next requests
  >> runKayle k e

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) (head . tail $ args)
  print configs

  let serverOpts = configGet configs serverInfoGet serverAddr_err_msg
  homer <- pickHomer' (C.addr serverOpts) (C.port serverOpts)

  -- Box initialization
  let dbOpts = configGet configs databaseGet db_err_msg
  bKey <- boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts)

  -- Database init, Create procTbl and historyTbl if not exists
  boxInit bKey

  let env = (KayleEnv configs manager args homer bKey)
  runKayle doKayle env

-- Append log message to Kayle
logKayle h = lift . appendLogger h

doKayle :: Kayle
doKayle = do
  env <- ask

  let homer = envHomer env
      bKey = envKey env

  logKayle "Info" "Ready to process requests"
  letter <- liftIO . waitHomer $ homer
  liftIO . print $ letter
  exists <- liftIO . isLetterExists bKey historyTbl $ (ident letter)
  if not exists
    then do procExists <- liftIO . isLetterExists bKey procTbl $ (ident letter)
            if not procExists
              then newLetter letter env
              else inProcLetter letter env
    else return ()
        -- Function to process new incomming letter
        -- fixme : Test may finished in this situation but
        --         it's not deal by this code.
  where newLetter :: Letter -> KayleEnv -> Kayle
        newLetter l env =
          let cfgs = envCfg env
              bKey = envKey env
          in (return $ letterInit cfgs l)
             >>= \x -> if isNothing x
                        then logKayle "Error" "letterInit failed"
                        else (logKayle "Info" $ "Insert letter Ident:"
                               ++ (H.ident l) ++ " Content: " ++ (show $ H.content l)
                               ++ " Into " ++ procTbl)
                             >> (liftIO . insertLetter bKey procTbl) (fromJust x)
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
          let content = H.content rl
             -- Update the letter from box
          in (return $ letterUpdate' bl
               [ (k, fromJust $ Map.lookup k content) | k <- allKeysOfContent rl])
             -- Put the letter from box back to box
             >>= (\x -> liftIO . updateLetter (envKey env) (H.ident x)
                        (encode $ H.content x) $ isTestFinished x)
             -- To check that whether the test describe by the letter is done
             >>= (\x -> if x == 1 && isTestSuccess rl
                        then liftIO . K.accept
                             (envMng env)
                             (envCfg env)
                             $ (fromJust $ retriFromHeader rl "iid")
                        else return ())


-- Generate a letter via exists letter and configuration
letterInit :: Configs -> Letter -> Maybe Letter
letterInit cfgs l = do
  let letter_ident = ident_name . str2Ident . ident $ l
      workContent_m = Map.lookup (trace letter_ident letter_ident) tContents
  if isNothing $ workContent_m
    then Nothing
    else let workContent = fromJust workContent_m
         in return $ Letter (ident l) (header l) $ fromList
            [ letterContentItem x (H.content l) |  x <- workContent ]
  where
    -- Contents of all testing projects
    tProjs = configGet cfgs (testPiecesGet (ident l)) test_proj_err_msg
    tContents = testContent tProjs
    -- All keys of content of letter
    allKeys = keys $ H.content l
    -- Item generator for content of new letter
    letterContentItem key content =
        if elem key allKeys
        then (key, lookup' key content)
        else (key, "O")
    lookup' k m = case Map.lookup k m of
                    Nothing -> "O"
                    Just v  -> v
