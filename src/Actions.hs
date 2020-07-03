{-# LANGUAGE OverloadedStrings  #-}

module Actions where

import Control.Exception
import KayleDefined as K
import LetterBox
import Letter
import Homer
import KayleBasics
import KayleConst
import Notifier
import Puller
import Utils

import Data.Maybe
import Control.Monad.Trans.Maybe
import Data.Map

import RepoOps

import Data.String.Conversions (cs)

type Action = Bool -> Letter -> KayleEnv -> IO Integer

mapFunc "F" = "Failed"
mapFunc "T" = "Successed"
-- Which should never happened
mapFunc _ = "Pending"
mapFunc' = \acc x -> acc ++ (fst x) ++ " -- " ++ (mapFunc . snd $ x) ++ "\n"

commitMessage :: KayleEnv -> Letter -> IO String
commitMessage env l = do
  let sha = ident_sha . str2Ident . ident $ l

  message <- commitMsg (envMng env) (envCfg env) sha
  -- The letter must exists
  letterMay <- runMaybeT $ searchLetter (envKey env) historyTbl (ident l)

  if isNothing letterMay
    then return ""
    else let letter = fromJust letterMay
             content_ = toList . content $ letter
             content' = Prelude.foldl mapFunc' "" content_
         in return $ message ++ "\n" ++ content'

shaFromLetter :: Letter -> String
shaFromLetter = ident_sha . str2Ident . ident

eventFromLetter :: Letter -> String
eventFromLetter = ident_event . str2Ident . ident

subject :: Letter -> String
subject l = "CI Information on commit " ++ "(" ++ (eventFromLetter l) ++ ")"  ++ " : " ++ (shaFromLetter l)

iid :: Letter -> String
iid l = let iidMaybe = retriFromHeader l "iid"
        in if isNothing iidMaybe
           then ""
           else fromJust iidMaybe

send_test_content :: KayleEnv -> Letter -> IO ()
send_test_content e l =
  (commitMessage e $ l)
  >>= (\body -> notifyViaEmail (envNotifier e) (subject l) body)

send_mr_result :: KayleEnv -> Letter -> IO ()
send_mr_result e l =
  emailBody >>= notifyViaEmail (envNotifier e) (subject l)

  where emailBody = do
          let iid = Actions.iid l
          body <- commitMessage e l

          return $ "MergeRequest: " ++ iid ++ "\n" ++ body

-- Action selector during the letter is new
actionSelector :: String -> Action
actionSelector event
  | event == "push" = push_action
  | event == "merge_request" = merge_action
  | event == "daily" = daily_action

-- Action definitions
daily_action :: Action
daily_action success l env =
  (commitMessage env $ l)
  >>= (\x -> if success
            then notifyViaEmail (envNotifier env) (subject l) x
            else (yesterday'sMR (envMng env) $ (envCfg env))
                 >>= (\mrs_today ->
                       notifyViaEmail (envNotifier env) (subject l) (x ++ mrs_today)))
  >> return (ident_sha . str2Ident $ ident l)
  >>= \sha -> retry retry_interval retry_count (put_req (req_url sha) (envMng env)) 0
  >> return k_ok

  where
    retry_interval = 1
    -- Retry untion success
    retry_count = 0
    req_url sha = "http://10.5.4.216:32359/manager/api/versions/" ++ sha  ++ "/temporaryGen/"

push_action :: Action
push_action success l env =
  send_test_content env l >> return k_ok

merge_action :: Action
merge_action success l env =
  -- (accept (envMng env) (envCfg env) (envNotifier env) $ (Actions.iid l))
  -- Have a request to Puller instead of accept directly
  if success
  then pullRequest (envPuller env) (Actions.iid l)
       >> send_mr_result env l
       >> return k_ok
  else send_mr_result env l
       >> return k_ok

-- Action selector during the letter already in history table.
actionSelector' :: String -> Action
actionSelector' event
  | event == "daily" = daily_action'
  | event /= "daily" = \s l e -> return k_ok

-- Action definitions

-- Create a temporary memory table and record received letters
-- after that just send email.
daily_action' :: Action
daily_action' success l env = do
  let ident_ = ident l
      bKey = envKey env

  -- Remove
  removeLetter bKey historyTbl ident_

  let initedLetter = letterInit (envCfg env) l

  if isNothing initedLetter
    -- Letter init failed which means the letter
    -- is not paired with the configuration be loaded.
    -- Just do nothing so return ok.
    then return k_ok
    else let initedLetter' = fromJust initedLetter
         in insertLetter bKey procTbl initedLetter'
            >> return k_ok
