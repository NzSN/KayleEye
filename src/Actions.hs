{-# LANGUAGE OverloadedStrings  #-}

module Actions where

import KayleDefined
import LetterBox
import Homer
import KayleBasics
import KayleConst

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
  let sha = Prelude.last . identSplit . ident $ l

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
shaFromLetter l = last . identSplit . ident $ l

subject :: Letter -> String
subject l = "CI Information on commit :" ++ (shaFromLetter l)

iid :: Letter -> String
iid l = let iidMaybe = retriFromHeader l "iid"
        in if isNothing iidMaybe
           then ""
           else fromJust iidMaybe

send_test_content :: KayleEnv -> Letter -> IO ()
send_test_content e l =
  (commitMessage e $ l)
  >>= (\body -> notify (cs body) (subject l) $ (envCfg e))

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
            then notify (cs x) (subject l) $ (envCfg env)
            else (yesterday'sMR (envMng env) $ (envCfg env))
                 >>= \mrs_today ->
                       notify (cs (x ++ mrs_today)) (subject l) $ (envCfg env))
  >> return k_ok

push_action :: Action
push_action success l env =
  send_test_content env l >> return k_ok

merge_action :: Action
merge_action success l env =
  (accept (envMng env) (envCfg env) $ (iid l))
  >> send_test_content env l
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
