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
import Data.Map as Map

-- List
import Data.List as List

-- Maybe
import Data.Maybe

-- Json
import Data.Aeson

-- Homer
import Homer as H
import LetterBox
import KayleConst
import KayleBasics as K

main = do
  -- Spawn http client manager
  manager <- newManager defaultManagerSettings

  -- Configuration file loaded
  args <- getArgs
  configs <- loadConfig (Prelude.head args) configPath

  -- Homer initialization
  let serverOpts = configGet configs serverInfoGet serverAddr_err_msg
  homer <- pickHomer' (C.addr serverOpts) (C.port serverOpts)

  -- Box initialization
  let dbOpts = configGet configs databaseGet db_err_msg
  bKey <- boxKeyCreate (C.db_host dbOpts) (C.db_user dbOpts) (C.db_pass dbOpts) (C.db dbOpts)

  -- Database init, Create procTbl and historyTbl if not exists
  boxInit bKey

  procRequests manager homer bKey configs


procRequests :: Manager -> Homer -> BoxKey -> Configs -> IO ()
procRequests mng homer key_ cfgs =
  let proc letter =
        -- Is letter already in history table, which means its already done previous
        (isLetterExists key_ historyTbl (ident letter)) >>= nextRequests''
        -- Is already in processing
        >> (isLetterExists key_ procTbl (ident letter))
        -- Processing the letter
        >>= (\isExists_proc -> if isExists_proc then (proc_in letter) else (proc_new letter))

      -- Function to Processing letters that in procTable
      proc_in letter =
        -- Search Letter from letterBox via the identity of received letter
        (runMaybeT $ searchLetter key_ procTbl (ident letter))
        -- If there is no such a letter in letterBox then just ignore the letter
        >>= (\x -> nextRequests' x >> (return $ fromJust x))
        -- Update the letter we just get from letterBox
        >>= (\x -> let content = H.content x
                   in return $ letterUpdate' x
                      -- lookup function must return non nothng value here
                      [ (k, fromJust $ Map.lookup k content) | k <- allKeysOfContent x ])
        -- To check that is the test recorded by the letter has been done
        -- Store the letter into proc table or history depend of the state of letter
        >>= (\x -> updateLetter key_ (H.ident x) (encode $ H.content x) $ isTestFinished x)
        >>= (\x -> if x == 1 && isTestSuccess letter
                      -- retriFromHeader must not return Nothing here
                      -- accept the merge request is such case
                   then K.accept mng cfgs (fromJust $ retriFromHeader letter "iid")
                        -- Notification when test failed is not need
                        -- case buildbot will do that.
                   else return () )
        -- After all pending for next request
        >> nextRequests

      -- Function to processing letters that first arrived
      proc_new letter =
        -- Generate an letter base on configuration and the received letter
        (return $ letterInit cfgs letter)
        -- If generation failed, which means the letter's project name can not match
        -- any test within configuration file, just ignore the letter and jump to next request
        >>= (\x -> nextRequests' x >> return x)
        -- Store the letter generated just now into letterBox
        >>= (\x -> insertLetter key_ procTbl (fromJust x))
        -- After all pending for next request
        >> nextRequests

  in print "Ready to process requests" >> (waitHomer homer) >>= proc

  where
    -- Immediately to wait to next requests
    nextRequests = procRequests mng homer key_ cfgs
    -- Immediately to wait to next requests, accept Maybe type
    nextRequests' x = if isNothing x then nextRequests else return ()
    -- Immediately to wait to next requests, accept Bool
    nextRequests'' x = if x then nextRequests else return ()

-- Generate a letter via exists letter and configuration
letterInit :: Configs -> Letter -> Maybe Letter
letterInit cfgs l = do
  let ident_ = str2Ident (ident l)

  if (ident_name ident_) == (testName tProj)
    then Nothing
    else return $ Letter (ident l) (header l)
         (fromList [ if elem x allKeys
                     then (x, lookup' x (H.content l))
                     else (x, "O")
                   | x <- tContents ])

  where tProj = configGet cfgs testPiecesGet test_proj_err_msg
        tContents = testContent tProj
        allKeys = keys $ H.content l
        lookup' k m = if isNothing $ Map.lookup k m then "O" else "T"
