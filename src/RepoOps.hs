module RepoOps where

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import KayleConst
import Modules.ConfigReader as Config

import Data.ByteString.Lazy

import Data.Maybe

import System.Environment
import KayleBasics

import Data.String.Conversions (cs)
import Control.Concurrent.Thread.Delay

acceptUrl = "http://gpon.git.com:8011/api/v4/projects/*/merge_requests/*/merge?private_token=*"
rebaseUrl = "http://gpon.git.com:8011/api/v4/projects/*/merge_requests/*/rebase?private_token=*"
commitsUrl = "http://gpon.git.com:8011/api/v4/projects/*/repository/commits/*?private_token=*"


-- Accept an merge request into main branch
accept :: Manager -> Configs
       -> String -- iid, Merge request id
       -> IO ()
accept mng cfgs iid = do
  let token = priToken $ configGet cfgs priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfgs listProjConfig "Project info not found"
      acceptUrl_ = replaceAll acceptUrl [projId, iid, token]
  code <- put_req acceptUrl_ mng

  case code of
    -- If merge request is unable to be accepted (ie: Work in Progress,
    -- Closed, Pipeline Pending Completion, or Failed while requiring Success) -
    -- you’ll get a 405 and the error message ‘Method Not Allowed’
    405 -> notify (cs $ "Merge request " ++ iid ++ " is unable to be accepted") "failed" cfgs
    -- If it has some conflicts and can not be merged - you’ll get a 406 and
    -- the error message ‘Branch cannot be merged’
    406 -> rebase mng cfgs iid >> delay (3 * seconds_micro) >> print "fixme"
    -- If the sha parameter is passed and does not match the HEAD of the source -
    -- you’ll get a 409 and the error message ‘SHA does not match HEAD of source branch’
    409 -> notify (cs $ "Merge request " ++ iid ++ ": SHA does not match HEAD of source branch")
           "failed"  cfgs
    -- If you don’t have permissions to accept this merge request - you’ll get a 401
    401 -> notify (cs $ "Permissions denied") "failed" cfgs
    -- Merge request not found or the url is wrong
    404 -> notify (cs "Merge requests not found or the url is wrong") "failed" cfgs
    -- Success
    200 -> return ()

-- Rebase merge request to target branch
rebase :: Manager -> Configs
       -> String -- iid, Merge request id
       -> IO ()
rebase mng cfgs iid = do
  let token = priToken $ configGet cfgs priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfgs listProjConfig "Project info not found"
      rebaseUrl_ = replaceAll acceptUrl [projId, iid, token]
  code <- put_req rebaseUrl_ mng

  case code of
    -- If you don’t have permissions to push to the merge request’s source branch -
    -- you’ll get a 403 Forbidden response.
    403 -> notify (cs "Permission denied to rebase merge request") "failed" cfgs
    -- The API will return a 202 Accepted response if the request is enqueued successfully
    -- Merge request not found or the url is wrong
    404 -> notify (cs $ "Merge requests(" ++ iid ++ ") not found or the url is wrong") "failed" cfgs
    -- Success
    202 -> return ()

-- Commit Message
commitMsg :: Manager -> Configs -> String -> IO String
commitMsg m cfg sha = do
  let token = priToken $ configGet cfg priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfg listProjConfig "Project info not found"
      commitUrl_ = replaceAll commitsUrl [projId, sha, token]

  response <- get_req commitUrl_ m

  case (fst response) of
    200 -> return $ getMsg (snd response)
    _   -> return ""
  where
    getMsg :: ByteString -> String
    getMsg l = do
      let o = (decode l :: Maybe Object)
      if isNothing o
        then ""
        else fromJust $ flip parseMaybe (fromJust o) $ \obj -> do
               msg <- obj .: cs "message"
               return msg
