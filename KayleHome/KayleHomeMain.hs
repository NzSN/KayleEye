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
import Control.Monad.Trans.Maybe

-- Socket
import Network.Socket

-- Map
import Data.Map

-- Homer
import Homer
import LetterBox
import KayleConst
import KayleBasics

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

  if isExists_history then nextRequests else return ()

  isExists_proc <- isLetterExists key_ procTbl (ident letter)
  if isExists_proc
    then do letter_ <- searchLetter_proc key_ (ident letter)
            nextRequests
    else insertLetter key_ procTbl letter >> nextRequests

  where nextRequests = procRequests homer key_ cfgs

        searchLetter_proc key ident_ = do
          letter_m <- runMaybeT $ searchLetter key procTbl ident_
          case letter_m of
            Nothing -> error "Letter not found"
            Just l -> return l

-- Generate a letter via exists letter and configuration
letterInit :: Configs -> Letter -> Maybe Letter
letterInit cfgs l = do
  let ident_ = str2Ident (ident l)

  if (ident_name ident_) == (testName tProj)
    then Nothing
    else return $ Letter (ident l) (fromList [(x, "F") | x <- tContents ])

  where tProj = configGet cfgs testPiecesGet test_proj_err_msg
        tContents = testContent tProj
