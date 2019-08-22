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

-- Homer
import Homer
import LetterBox
import KayleConst
import KayleBasics

-- Monad Transformers
import Control.Monad.Trans.Maybe

-- Socket
import Network.Socket

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath

  -- Homer initialization
  let serverOpts = configGet configs serverInfoGet serverAddr_err_msg
  homer <- pickHomer (C.addr serverOpts) (C.port serverOpts)

  -- Box initialization
  let dbOpts = configGet configs databaseGet db_err_msg
  bKey <- boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts)

  -- Database init, Create procTbl and historyTbl if not exists
  boxInit bKey

  procRequests homer bKey configs


procRequests :: Homer -> BoxKey -> Configs -> IO ()
procRequests homer key_ cfgs = do
  letter <- waitHomer homer

  isExists_history <- isLetterExists key_ historyTbl (ident letter)
  nextRequests isExists_history

  isExists_proc <- isLetterExists key_ procTbl (ident letter)
  if isExists_proc
    then do letter_ <- searchLetter_proc key_ (ident letter)
            nextRequests True
    else insertLetter key_ procTbl letter >> nextRequests True

  where nextRequests True = procRequests homer key_ cfgs
        nextRequests False = return ()

        searchLetter_proc key ident_ = do
          letter_m <- runMaybeT $ searchLetter key procTbl ident_
          case letter_m of
            Nothing -> error "Letter not found"
            Just l -> return l

-- Generate a letter via exists letter and configuration
--letterInit :: Configs -> Letter -> Letter
--letterInit cfgs l =
--  let tProj = configGet cfgs testPiecesGet test_proj_err_msg
